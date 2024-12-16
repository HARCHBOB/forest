{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( Lib3.stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    renderQuery,
    renderTree,
    renderName,
    ) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, writeChan, readChan )
import Control.Concurrent.STM(TVar, atomically, readTVar, writeTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStr)
import System.Directory (doesFileExist)
import Lib2
import Parsers
import qualified Parsers as Lib2
import Debug.Trace (trace)

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
parseCommand str = case parse (
  StatementCommand <$> 
  try statements <|> 
  try parseLoad <|> 
  parseSave) str of
    (Left e, _) -> Left e
    (Right c, r) -> Right (c, r)

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
      _ <- parseWhitespace
      q <-
        many
          ( do
              q <- Lib2.parseCommands
              _ <- parseLiteral ";"
              _ <- parseWhitespace
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseWhitespace
      return $ Batch q
  )
    <|> (Single <$> Lib2.parseCommands)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements str = case parse statements str of
  (Left e, _) -> Left e
  (Right s, r) -> Right (s, r)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch $ map PlantTree (forest state)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) queries ++ "END\n"

renderQuery :: Lib2.Query -> String
renderQuery (PlantTree tree) = "plant " ++ renderTree tree

renderTree :: Lib2.Tree -> String
renderTree (Lib2.Tree name branches) = renderName name ++ renderBranches branches

renderName :: Lib2.Name -> String
renderName Lib2.Oak = "oak"
renderName Lib2.Pine = "pine"
renderName Lib2.Birch = "birch"
renderName Lib2.Maple = "maple"

renderBranch :: Lib2.Branch -> String
renderBranch (Lib2.Branch leaves) = "branch" ++ renderLeaves leaves

renderBranches :: Lib2.Branches -> String
renderBranches Lib2.None = " none"
renderBranches (Lib2.SingleBranch branch) = " " ++ renderBranch branch
renderBranches (Lib2.MultipleBranches branch branches) = " " ++ renderBranch branch ++ renderBranches branches

renderLeaf :: Lib2.Leaf -> String
renderLeaf Lib2.Leaf = "leaf"

renderLeaves :: Lib2.Leaves -> String
renderLeaves (Lib2.SingleLeaf leaf) = " " ++ renderLeaf leaf
renderLeaves (Lib2.MultipleLeaves leaf leaves) = " " ++ renderLeaf leaf ++ renderLeaves leaves

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
  SaveCommand -> do
    state <- readTVarIO tvarState
    let sts = marshallState state
    liftIO $ withFile "state.txt" WriteMode $ \h -> do
      hPutStr h (renderStatements sts)
    return $ Right (Just "State saved successfully")
  LoadCommand -> do
    contents <- readFile "state.txt"
    case parseStatements contents of
      Right (sts, _) -> atomically $ do
        state <- readTVar tvarState
        let queries = case sts of
                        Batch qs -> qs
                        Single q -> [q]
        let results = foldl (\acc query -> case acc of
                                             Left err -> Left err
                                             Right (_, st) -> Lib2.stateTransition st query) (Right (Nothing, state)) queries
        case results of
          Right (_, newState) -> do
            writeTVar tvarState newState
            case Lib2.stateTransition newState InspectForest of
              Right (Just msg, _) -> return $ Right (Just ("State loaded successfully.\n" ++ msg))
              Right (Nothing, _) -> return $ Right (Just "State loaded successfully.")
              Left err -> return $ Left err
          Left err -> return $ Left "Failed to load state"
      Left err -> return $ Left err
  StatementCommand sts -> atomically $ do
    state <- readTVar tvarState
    let queries = case sts of
                    Batch qs -> qs
                    Single q -> [q]
    let results = foldl (\acc query -> case acc of
                                         Left err -> Left err
                                         Right (_, st) -> Lib2.stateTransition st query) (Right (Nothing, state)) queries
    case results of
      Right (msg, newState) -> do
        writeTVar tvarState newState
        return $ Right msg
      Left err -> return $ Left "Failed to execute batch"
