{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.String.Conversions (cs)
import qualified Lib2
import qualified Lib3
import Web.Scotty

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan

  stopChan <- newEmptyMVar

  serverThread <- forkIO $ scotty 3000 $ do
    post "/" $ do
      requestBody <- body
      liftIO $ putStrLn ("Request was: " ++ cs requestBody)
      response <- liftIO $ parseInput state chan $ cs requestBody
      text $ cs response
    
    post "/stop" $ do
      liftIO $ putStrLn "Stop endpoint called. Shutting down..."
      liftIO $ putMVar stopChan ()
      text "Server is shutting down."
    
  -- Wait for the stop signal
  takeMVar stopChan
  putStrLn "Server is shutting down..."

  -- Kill the server thread
  killThread serverThread
  putStrLn "Server has been stopped."
  

parseInput :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
parseInput state storageChan input =
  let commands = splitCommands input
  in if null commands
      then return "No commands to execute."
      else do
        results <- forM commands $ \cmd ->
          case Lib3.parseCommand cmd of
            Left err -> return $ "PARSE ERROR: " ++ err ++ " | Command: " ++ cmd
            Right (command, remaining) ->
              if not (null remaining)
                then return $ "PARSE ERROR: Unexpected input after command: " ++ cmd ++ " | Remaining: " ++ remaining
                else do
                  putStrLn ("Command was: " ++ cmd)
                  putStrLn ("Command interpretation: " ++ show command)
                  result <- Lib3.stateTransition state command storageChan
                  formatResult result
        return $ unlines results
  where
    -- | Splits the input text into individual commands based on the delimiter ';'
    splitCommands :: String -> [String]
    splitCommands = map trim . filter (not . null) . splitOn ';'

    -- | Splits a string based on a delimiter.
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn delim str =
      let (before, remainder) = span (/= delim) str
      in before : case remainder of
                   [] -> []
                   (_:rest) -> splitOn delim rest

    -- | Trims leading and trailing whitespace from a string.
    trim :: String -> String
    trim = f . f
      where f = reverse . dropWhile isSpace

    formatResult :: Either String (Maybe String) -> IO String
    formatResult (Left errMsg) = do
      putStrLn $ "Result: EXECUTION ERROR: " ++ errMsg
      return $ "EXECUTION ERROR: " ++ errMsg
    formatResult (Right maybeMsg) = case maybeMsg of
      Nothing -> do
        putStrLn "Result: Success"
        return "Success"
      Just msg -> do
        putStrLn "Result:"
        mapM_ (putStrLn . ("  " ++)) (lines msg)
        return msg