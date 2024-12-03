{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( Lib3.stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent ( Chan, writeChan, readChan )
import Control.Concurrent.STM(STM, TVar, atomically, modifyTVar', readTVar, writeTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStr)
import System.Directory (doesFileExist)
import Data.Either (isRight, rights)
import Lib2
import Parsers
import qualified Parsers as Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
      op <- readChan chan
      case op of
        Save content responseChan -> do
          writeFile "state.txt" content
          writeChan responseChan ()
          loop
        Load responseChan -> do
          fileExists <- doesFileExist "state.txt"
          if fileExists
            then do
              content <- withFile "state.txt" ReadMode hGetContents
              writeChan responseChan content
            else writeChan responseChan ""
          loop

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

parseLoad :: Parser Command
parseLoad = do
  _ <- parseLiteral "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseLiteral "save"
  return SaveCommand

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN"
      _ <- parseLiteral "\n"
      q <-
        many
          ( do
              q <- Lib2.parseCommands
              _ <- parseLiteral ";"
              _ <- parseLiteral "\n"
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseLiteral "\n"
      return $ Batch q
  )
    <|> (Single <$> Lib2.parseCommands)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch [PlantForest (Lib2.forest state)]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Batch queries) = unlines (map show queries)
renderStatements (Single query) = show query

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition tvarState command ioChan = case command of
  StatementCommand statements -> atomically $ do
    state <- readTVar tvarState
    let queries = case statements of
                    Batch qs -> qs
                    Single q -> [q]
    let results = map (Lib2.stateTransition state) queries
    if all isRight results
      then do
        let newState = foldl (\_ result -> case result of
                                             Right (_, st) -> st
                                             Left _ -> state) state results
        writeTVar tvarState newState
        return $ Right (Just ("Batch executed successfully" ++ " " ++ show newState))
      else return $ Left "Failed to execute batch"
  LoadCommand -> do
    contents <- readFile "state.txt"
    case parseStatements contents of
      Right (statements, _) -> atomically $ do
        state <- readTVar tvarState
        let queries = case statements of
                        Batch qs -> qs
                        Single q -> [q]
        let results = map (Lib2.stateTransition state) queries
        if all isRight results
          then do
            let newState = foldl (\s result -> case result of
                                                Right (_, st) -> st
                                                Left _ -> s) state results
            writeTVar tvarState newState
            return $ Right (Just "State loaded successfully")
          else return $ Left "Failed to load state"
      Left err -> return $ Left err
  SaveCommand -> do
    state <- readTVarIO tvarState
    let statements = marshallState state
    liftIO $ withFile "state.txt" WriteMode $ \h -> do
      hPutStr h (renderStatements statements)
    return $ Right (Just "State saved successfully")
