{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Middleware.LoginWithTwitter
  ( Config(..)
  , middleware
  , TwitterLoginException(..)
  , TwitterLoginResult(..)
  ) where

import Network.Wai
import Control.Monad (when, join)
import Control.Applicative ((<|>))
import Control.Exception
import Data.IORef
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Builder as Bu
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Char8 (pack)
import qualified Data.Vault.Lazy as V
import Network.HTTP.Types.Status (found302)
import Network.HTTP.Types.Method (methodGet, methodHead, methodPost)
import Network.HTTP.Types.Header (hCookie, hSetCookie, hLocation)
import qualified Web.Cookie as Ck
import qualified Data.Text as T
import qualified Data.Map as M
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Web.Authenticate.OAuth as OA
import qualified Web.Twitter.Conduit     as Tw
import qualified Web.Twitter.Conduit.Api as Tw
import qualified Web.Twitter.Types       as Tw

-- | LoginPath を CallbackPath は同じパスは許可しない
-- | e.g. login path    /auth/twitter
-- |      callback path /auth/twitter/callback
-- | origin は適切な callback url を構成するのに必要(ユーザが見る origin であり、リバースプロキシの際は注意)
-- |
-- | 事前に用意すること
-- |
-- |  * twitter App で「認証を有効にしておく」こと、
-- |  * 無駄な権限要求を外す。
-- |  * callback url を登録する(http://localhost:8080 とかも受け付けてくれるので手元で動作確認できる)
-- |
-- | ユーザが実装するべきこと。
-- |
-- |  * <login url> に GET / POST することで twitterの認証画面に遷移する
-- |  * GET <callback url> で認証の結果を待ち受ける(それ以外は通さない)。リダイレクトで twitter
-- |    認証画面からクエリパラメータが付いているめ、リダイレクトするべし

-- origin の名前は以下参照
-- https://stackoverflow.com/questions/2081645/what-do-you-call-the-entire-first-part-of-a-url
data Config = Config
  { configOrigin :: Text        -- ^ http://foo.home.jp
  , configLoginPath :: Text     -- ^ /twitter/login
  , configCallbackPath :: Text  -- ^ /twitter/login
  , configConsumerKey :: ByteString
  , configConsumerSecret :: ByteString
  }

data TwitterLoginException
  = InvalidTemporarlCredential
  | CallbackConfirmedFailed
  deriving (Eq, Show)

instance Exception TwitterLoginException

-- TODO: 整理する必要あり
data TwitterLoginResult
  = Success Tw.User        -- ^ ユーザが許可した
  | Denied                 -- ^ ユーザに拒否した
  | ProblemOccured         -- ^ twitterの認証画面からは返ってきたが、認証もキャンセルでもない(？？)
  | SessionExpired         -- ^ 規定時間内に認証押してくれなかった(攻撃も含まれる)
  | NoToken                -- ^ リクエストトークンの不在(殆どの場合、攻撃かな)
  | InvalidToken           -- ^ 不正なリクエストトークン(攻撃、もしくは既にそのトークンを利用した場合)
  deriving (Show)

-- ユーザに callbacks url はドメインも含め指定してもらう必要がある。
-- リバースプロキシの裏で動作する可能性があるので request からはドメイン
--
-- 疑問: twitterアカウントって破棄した場合、別の人がそのアカウントを際取得できる？
--       であれば アカウント名で紐付けはまずいよね。多分一意的なIDがあるはず
-- 疑問: Credential っていつまで有効なんだろ？DBの保持していいいのかな？そもそも意味ある？
--
-- 懸念
--  * manager のリソースが管理されていない
--  * credMap にゴミが溜っていく可能性あり
--
-- 疑問 tmpCred は必要なのか？ ユーザが認証画面から返ってきた時、
-- request_token, request_token_secretは HTTP リクエストの情報から取れ
-- るので不要では？という気がするが...ただ twitter のドキュメントに確
-- 認しろ!ってあるんだよね...
--
-- Upon a successful authentication, your callback_url would receive a
-- request containing the oauth_token and oauth_verifier
-- parameters. Your application should verify that the token matches
-- the request token received in step 1.
--
-- request_token + request_token_secret の秘密の鍵によるダイジェストを
-- cookie の中に一緒に入れれば大丈夫かな？
middleware :: Config -> IO (Request -> IO TwitterLoginResult, Middleware)
middleware config@Config{..} = do
  vkey <- V.newKey
  secretMapRef <- newIORef mempty
  manager <- newManager tlsManagerSettings
  let callbackUrl = encodeUtf8 $ configOrigin <> configCallbackPath
  let oauth = mkTwitterOAuth configConsumerKey configConsumerSecret callbackUrl
  let getResult req = V.lookup vkey (vault req) & maybeThrowIO (ErrorCall "Called on wrong route.")
  pure (getResult, middleware' config vkey secretMapRef manager oauth)

-- ユーザが認証画面で不許可(「キャンセル」を選択)にした場合
-- twitter の 開発者用ドキュメントに書かれていないが、認証画面で「キャンセル」>「<app>に戻る」を選択すると、
-- <callback url>?denied=<request token> にリダイレクトされる。
--
-- callback URL の HEAD要求はサポートしない(というかしては駄目な気がする)
middleware'
  :: Config
  -> V.Key TwitterLoginResult
  -> IORef (M.Map ByteString (ByteString, Tw.Credential))
  -> Tw.Manager
  -> Tw.OAuth
  -> Middleware
middleware' Config{..} vkey secretMapRef manager oauth app req respond
  | pathIs configLoginPath req && (isGet req || isPost req) = do
      -- Get request token and redirect user to twitter login page
      tmpCred <- OA.getTemporaryCredential oauth manager
      TTC{..} <- extractTTC tmpCred & maybeThrowIO InvalidTemporarlCredential
      when (not ttcCallbackConfirmed) $ throwIO CallbackConfirmedFailed
      modifyIORef secretMapRef $ M.insert ttcRequestToken (ttcRequestTokenSecret, tmpCred)
      let authUrl = pack $ OA.authorizeUrl oauth tmpCred
      let cookie = renderCookieBS $ mkCookie ttcRequestTokenSecret
      respond $ responseLBS found302 [(hSetCookie, cookie), (hLocation, authUrl)] ""
  | pathIs configCallbackPath req && isGet req = do
      -- Came back from Twtter Login.
      secretMap <- readIORef secretMapRef
      let qs = queryString req
      let deniedToken'         = join $ lookup "denied" qs
      let token'               = join $ lookup "oauth_token" qs
      let verifier'            = join $ lookup "oauth_verifier" qs
      let cookieTokenSecret'   = lookup hCookie (requestHeaders req) >>= lookup cookieName . Ck.parseCookies
      let expectedTokenSecret' = token' >>= flip M.lookup secretMap
      result <-
        case token' of
          Just token ->
            case (cookieTokenSecret', expectedTokenSecret') of
              (Just cookieTokenSecret, Just (expectedTokenSecret, tmpCred))
                | cookieTokenSecret == expectedTokenSecret ->
                    case verifier' of
                      Just verifier -> handlePermit oauth manager tmpCred verifier
                      Nothing       -> pure ProblemOccured
                | otherwise         -> pure InvalidToken
              (Just _ , Nothing)    -> pure InvalidToken
              (Nothing, _      )    -> pure SessionExpired
          Nothing ->
            case deniedToken' of
              Just _                -> pure Denied
              Nothing               -> pure NoToken
      -- 失敗した場合でも消すべきなのか？疑問
      -- 不許可にした場合でも
      whenJust_ (token' <|> deniedToken') $ modifyIORef' secretMapRef . M.delete
      -- TODO: need to unset cookie
      app (setResult vkey result req) respond
  | otherwise =
      -- delegate
      app req respond
  where
    -- ユーザが許可した
    -- NOTE: Authenticate で得られた Credentials って accountVerifyCredentials しか取れない？
    handlePermit oauth manager tmpCred verifier = do
      cred <- OA.getAccessToken oauth (OA.injectVerifier verifier tmpCred) manager
      let twInfo = Tw.def { Tw.twToken = Tw.def { Tw.twOAuth = oauth, Tw.twCredential = cred } }
      user <- Tw.call twInfo manager Tw.accountVerifyCredentials
      pure $ Success user

    setResult vkey result req =
      req { vault = V.insert vkey result (vault req) }

    pathIs path req = ("/" <> T.intercalate "/" (pathInfo req)) == path
    isGet req = requestMethod req == methodGet
    isPost req = requestMethod req == methodPost

    cookieName = "twitter-oauth-request-token-secret"

    -- expires within 10 minutes
    mkCookie value = Ck.defaultSetCookie
      { Ck.setCookieName = cookieName
      , Ck.setCookieValue = value
      , Ck.setCookieHttpOnly = True
      , Ck.setCookieSecure = T.isPrefixOf "https://" configOrigin
      , Ck.setCookieMaxAge = Just 600
      }

    renderCookieBS = toStrict . Bu.toLazyByteString . Ck.renderSetCookie


-- https://developer.twitter.com/en/docs/twitter-for-websites/log-in-with-twitter/guides/implementing-sign-in-with-twitter
mkTwitterOAuth
  :: ByteString   -- ^ Consumer Key
  -> ByteString   -- ^ Consumer Secret
  -> ByteString   -- ^ Callback URL
  -> OA.OAuth
mkTwitterOAuth consumerKey consumerSecret callbackUrl = OA.newOAuth
  { OA.oauthServerName     = "api.twitter.com"
  , OA.oauthRequestUri     = "https://api.twitter.com/oauth/request_token"
  , OA.oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
  , OA.oauthAuthorizeUri   = "https://api.twitter.com/oauth/authenticate"
  , OA.oauthCallback       = Just callbackUrl
  , OA.oauthConsumerKey    = consumerKey
  , OA.oauthConsumerSecret = consumerSecret
  }

-- Twitter Temporal Credential
data TTC = TTC
  { ttcRequestToken :: ByteString
  , ttcRequestTokenSecret :: ByteString
  , ttcCallbackConfirmed :: Bool
  }

extractTTC
  :: OA.Credential
  -> Maybe TTC
extractTTC (OA.Credential cr) =
  TTC
    <$> lookup "oauth_token" cr
    <*> lookup "oauth_token_secret" cr
    <*> (toBool <$> lookup "oauth_callback_confirmed" cr)
  where
    toBool "true" = True
    toBool _ = False

maybeThrowIO :: Exception e => e -> Maybe a -> IO a
maybeThrowIO _ (Just a) = pure a
maybeThrowIO e Nothing  = throwIO e

whenJust_ :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust_ (Just a) m = (m a)
whenJust_ Nothing  _ = pure ()
