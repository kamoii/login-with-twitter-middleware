{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude()
import Relude hiding (get)
import System.Environment (getEnv)
import Web.Scotty
import qualified Network.Wai.Middleware.LoginWithTwitter as LWT
import qualified Data.List as L
import qualified Data.ByteString.Builder as BSB
import Lucid
import Web.Cookie as Ck
import qualified Web.Twitter.Types as Tw
import qualified Data.Time.Clock as Cl
import qualified Data.Time.Calendar as Cl

-- | Set http://localhost:8080/login-with-twitter as valid callbacl url
--
main :: IO ()
main = do
  (consumerKey, consumerSecret) <- readTwitterConsumerVals
  (getLoginResult, lwtMiddleware) <- LWT.middleware (mkLWTConfig consumerKey consumerSecret)

  scotty 8080 $ do
    middleware lwtMiddleware

    get "/" do
      userMaybe <- checkSession
      html . renderText $ content_ do
        h1_ "Test App for login-with-twitter"
        case userMaybe of
          Just userName -> do
            p_ . toHtml $ "Hello, " <> userName
            form_ [method_ "POST", action_ "/logout"] $ button_ "logout"
          Nothing ->
            form_ [method_ "POST", action_ loginPath] $ button_ "login with twitter"

    get callbackRoute do
      loginResult <- liftIO . getLoginResult =<< request
      case loginResult of
        LWT.Success user -> do
          -- setSession . encodeUtf8 $ Tw.userScreenName user <> "@" <>
          setSession . encodeUtf8 $ Tw.userName user <> "@" <> Tw.userScreenName user
          redirect "/"
        _ ->
          redirect "/"

    post "/logout" do
      deleteSession
      redirect "/"

  where
    loginPath = "/auth/twitter"
    callbackPath = "/auth/twitter/callback"
    callbackRoute = fromString . toString $ callbackPath

    content_ :: Html () -> Html ()
    content_ con =
      html_ do
        head_ do
          title_ "login-with-twitter"
        body_ do
          main_ con

    readTwitterConsumerVals = do
      key <- getEnv "TWITTER_CONSUMER_KEY"
      secret <- getEnv "TWITTER_CONSUMER_SECRET"
      pure (key, secret)

    mkLWTConfig consumerKey consumerSecret = LWT.Config
      { LWT.configOrigin = "http://localhost:8080"
      , LWT.configLoginPath = loginPath
      , LWT.configCallbackPath = callbackPath
      , LWT.configConsumerKey = encodeUtf8 consumerKey
      , LWT.configConsumerSecret = encodeUtf8 consumerSecret
      }

    sessionKey = "login-with-twitter-name"

    checkSession = do
      ck <- header "cookie"
      pure $ Ck.parseCookies . encodeUtf8 <$> ck >>= L.lookup sessionKey

    setSession name = do
      let ck = Ck.defaultSetCookie
            { Ck.setCookieName = sessionKey
            , Ck.setCookieValue = name
            , Ck.setCookiePath = Just "/"
            , Ck.setCookieMaxAge = Just 600
            , Ck.setCookieHttpOnly = True
            }
      setHeader "Set-Cookie"
        . decodeUtf8
        . BSB.toLazyByteString
        $ Ck.renderSetCookie ck

    deleteSession = do
      let past = Cl.UTCTime (Cl.ModifiedJulianDay 0) 0
      let ck = Ck.defaultSetCookie
            { Ck.setCookieName = sessionKey
            , Ck.setCookieValue = ""
            , Ck.setCookiePath = Just "/"
            , Ck.setCookieExpires = Just past
            , Ck.setCookieHttpOnly = True
            }
      setHeader "Set-Cookie"
        . decodeUtf8
        . BSB.toLazyByteString
        $ Ck.renderSetCookie ck
