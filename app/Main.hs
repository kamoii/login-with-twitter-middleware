{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude()
import Relude hiding (get)
import System.Environment (getEnv)
import Web.Scotty
import qualified Network.Wai.Middleware.LoginWithTwitter as LWT
import Lucid

-- | Set http://localhost:8080/login-with-twitter as valid callbacl url
--
main :: IO ()
main = do
  (consumerKey, consumerSecret) <- readTwitterConsumerVals
  (getLoginResult, lwtMiddleware) <- LWT.middleware (mkLWTConfig consumerKey consumerSecret)

  scotty 8080 $ do
    middleware lwtMiddleware

    get "/" do
      html . renderText $ content_ do
        h1_ "Test App for login-with-twitter"
        a_ [href_ loginPath] $ button_ "login with twitter"

    get callbackRoute do
      loginResult <- liftIO . getLoginResult =<< request
      case loginResult of
        LWT.Success user ->
          pure ()
        _ ->
          pure ()

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
