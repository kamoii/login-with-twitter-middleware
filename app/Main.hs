{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude()
import qualified Data.Vault.Lazy as V
import Relude hiding (get)
import System.Environment (getEnv)
import Web.Scotty
import qualified Network.Wai.Middleware.LoginWithTwitter as LWT
import qualified Network.Wai as W
import Lucid

-- | Set http://localhost:8080/login-with-twitter as valid callbacl url
--
main :: IO ()
main = do
  (consumerKey, consumerSecret) <- readTwitterConsumerVals
  (vkey, lwtMiddleware) <- LWT.middleware (mkLWTConfig consumerKey consumerSecret)

  scotty 8080 $ do
    middleware lwtMiddleware

    get "/" do
      html . renderText $ content_ do
        h1_ "Test App for login-with-twitter"
        p_ "Currently not logined."
        a_ [href_ "/login-with-twitter"] $ button_ "login with twitter"

    post "/login-with-twitter" do
      req <- request
      case V.lookup vkey (W.vault req) of
        Just (LWT.Success user) ->
          pure ()
        Just _ ->
          pure ()
        Nothing ->
          -- shoudn't occure
          pure ()

  where
    content_ :: Html () -> Html ()
    content_ con =
      html_ do
        head_ do
          title_ "login-with-twitter"
          link_ [rel_ "stylesheet",  href_ "https://unpkg.com/marx-css/css/marx.min.css" ]
        body_ do
          main_ con

    readTwitterConsumerVals = do
      key    <- getEnv "TWITTER_CONSUMER_KEY"
      secret <- getEnv "TWITTER_CONSUMER_SECRET"
      when (key == "" || secret == "") do
        putStrLn "Set TWITTER_CONSUMER_KEY and TWITTER_CONSUMER_SECRET environment variables."
        exitFailure
      pure (key, secret)

    mkLWTConfig consumerKey consumerSecret = LWT.Config
      { LWT.configOrigin = "http://localhost:8080"
      , LWT.configLoginPath = "/login-with-twitter"
      , LWT.configCallbackPath = "/login-with-twitter"
      , LWT.configConsumerKey = encodeUtf8 consumerKey
      , LWT.configConsumerSecret = encodeUtf8 consumerSecret
      }
