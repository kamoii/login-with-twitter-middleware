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

-- | Set http://localhost:8080/login-with-twitter as valid callbacl url
main :: IO ()
main = do
  (consumerKey, consumerSecret) <- readConsumerVals
  (vkey, lwtMiddleware) <- LWT.middleware (mkLWTConfig consumerKey consumerSecret)

  scotty 8080 $ do
    middleware lwtMiddleware

    get "/" do
      pure ()

    post "/login-with-twitter" do
      req <- request
      case V.lookup vkey (W.vault req) of
        Just (LWT.Success user) ->
          pure ()
        Just _ ->
          pure ()
        Nothing ->
          pure ()

  where
    readConsumerVals = do
      consumerKey <- getEnv "TWITTER_CONSUMER_KEY"
      consumerSecret <- getEnv "TWITTER_CONSUMER_SECRET"
      when (consumerKey == "" || consumerSecret == "") do
        putStrLn "Set TWITTER_CONSUMER_KEY and TWITTER_CONSUMER_SECRET environment variables."
        exitFailure
      pure (consumerKey, consumerSecret)

    mkLWTConfig consumerKey consumerSecret = LWT.Config
      { LWT.configOrigin = "http://localhost:8080"
      , LWT.configLoginPath = "/login-with-twitter"
      , LWT.configCallbackPath = "/login-with-twitter"
      , LWT.configConsumerKey = encodeUtf8 consumerKey
      , LWT.configConsumerSecret = encodeUtf8 consumerSecret
      }
