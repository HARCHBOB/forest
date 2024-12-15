{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Lib2
import qualified Lib3
import Web.Scotty

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  scotty 3000 $
    post "/" $ do
      requestBody <- body
      liftIO $ putStrLn ("Request was: " ++ cs requestBody)
      response <- liftIO $ parseInput state chan $ cs requestBody
      text $ cs response

parseInput :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
parseInput state storageChan input =
  case Lib3.parseCommand input of
    Left err -> return $ "PARSE ERROR: " ++ err
    Right (command, "") -> do
      result <- Lib3.stateTransition state command storageChan
      case result of
        Left err -> return $ "EXECUTION ERROR: " ++ err
        Right output -> do
          putStrLn $ "Server response: " ++ fromMaybe "Success" output
          return $ fromMaybe "Success" output
    Right (_, remaining) -> return $ "PARSE ERROR: Unexpected input: " ++ remaining
