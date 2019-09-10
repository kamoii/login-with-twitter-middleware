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
            p_ . toHtml $ "Hello" <> userName
          Nothing ->
            form_ [action_ loginPath] $ button_ "login with twitter"

    get callbackRoute do
      loginResult <- liftIO . getLoginResult =<< request
      case loginResult of
        LWT.Success user -> do
          setSession . encodeUtf8 $ Tw.userScreenName user <> "@" <> Tw.userName user
          redirect "/"
        _ ->
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
          link_ [rel_ "stylesheet",  href_ "https://unpkg.com/marx-css/css/marx.min.css" ]
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
            , Ck.setCookieMaxAge = Just 600
            , Ck.setCookieHttpOnly = True
            }
      setHeader "set-cookie"
        . decodeUtf8
        . BSB.toLazyByteString
        $ Ck.renderSetCookie ck
