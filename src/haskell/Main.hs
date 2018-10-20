{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import qualified Server

main :: IO ()
main = do
  Server.run


  -- -- forever $ do
  --    xs <- words <$> getLine
  --    case xs of
  --      [name', email'] -> do
  --        etoken <- makeJWT (User name' email') jwtCfg Nothing
  --        case etoken of
  --          Left e ->
  --            putStrLn $ "Error generating token:t" ++ show e
  --          Right v ->
  --            putStrLn $ "New token:\t" ++ show v
  --      _ ->
  --        putStrLn "Expecting a name and email separated by spaces"
