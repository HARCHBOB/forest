{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.String.Conversions (cs)
import Network.Wreq
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import System.Environment (getArgs)
import Control.Lens ((^.))
import qualified Lib2
import qualified Lib3
import qualified Parsers

-- Command Data Type
data Command next
  = PlantTree String next
  | CutBranch String next
  | CutBranches String next
  | CutTree String next
  | CutForest next
  | InspectBranch String (String -> next)
  | InspectBranches String (String -> next)
  | InspectTree String (String -> next)
  | InspectForest (String -> next)
  | Save next
  | Load (String -> next)
  deriving (Functor)

type ForestDSL = Free Command

-- Smart Constructors
save :: ForestDSL ()
save = liftF $ Save ()

load :: ForestDSL String
load = liftF $ Load id

plantTree :: String -> ForestDSL ()
plantTree tree = liftF $ PlantTree tree ()

cutBranch :: String -> ForestDSL ()
cutBranch name = liftF $ CutBranch name ()

cutBranches :: String -> ForestDSL ()
cutBranches name = liftF $ CutBranches name ()

cutTree :: String -> ForestDSL ()
cutTree name = liftF $ CutTree name ()

cutForest :: ForestDSL ()
cutForest = liftF $ CutForest ()

inspectBranch :: String -> ForestDSL String
inspectBranch name = liftF $ InspectBranch name id

inspectBranches :: String -> ForestDSL String
inspectBranches name = liftF $ InspectBranches name id

inspectTree :: String -> ForestDSL String
inspectTree name = liftF $ InspectTree name id

inspectForest :: ForestDSL String
inspectForest = liftF $ InspectForest id

-- Interpreter that sends HTTP requests per command (Single)
runHttpSingle :: ForestDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (PlantTree tree next)) = do
  _ <- post "http://localhost:3000" (cs $ "plant " ++ tree :: ByteString)
  runHttpSingle next

runHttpSingle (Free (CutBranch name next)) = do
  _ <- post "http://localhost:3000" (cs $ "cut branch " ++  name :: ByteString)
  runHttpSingle next

runHttpSingle (Free (CutBranches name next)) = do
  _ <- post "http://localhost:3000" (cs $ "cut branches " ++  name :: ByteString)
  runHttpSingle next

runHttpSingle (Free (CutTree name next)) = do
  _ <- post "http://localhost:3000" (cs $ "cut " ++  name :: ByteString)
  runHttpSingle next

runHttpSingle (Free (CutForest next)) = do
  _ <- post "http://localhost:3000" (pack "cut forest" :: ByteString)
  runHttpSingle next

runHttpSingle (Free (InspectBranch name next)) = do
  resp <- post "http://localhost:3000" (cs $ "inspect branch " ++  name :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (InspectBranches name next)) = do
  resp <- post "http://localhost:3000" (cs $ "inspect branches " ++  name :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (InspectTree name next)) = do
  resp <- post "http://localhost:3000" (cs $ "inspect " ++  name :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (InspectForest next)) = do
  resp <- post "http://localhost:3000" (pack "inspect forest" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (Save next)) = do
  _ <- post "http://localhost:3000" (pack "save" :: ByteString)
  runHttpSingle next

runHttpSingle (Free (Load next)) = do
  resp <- post "http://localhost:3000" (pack "load" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)


-- Interpreter that batches commands
runHttpBatch :: ForestDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> ForestDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  return a

runHttpBatch' acc (Free (PlantTree tree next)) =
  runHttpBatch' (acc ++ ["plant " ++ tree]) next

runHttpBatch' acc (Free (CutBranch name next)) =
  runHttpBatch' (acc ++ ["cut branch " ++  name]) next

runHttpBatch' acc (Free (CutBranches name next)) =
  runHttpBatch' (acc ++ ["cut branches " ++  name]) next

runHttpBatch' acc (Free (CutTree name next)) =
  runHttpBatch' (acc ++ ["cut " ++  name]) next

runHttpBatch' acc (Free (CutForest next)) =
  runHttpBatch' (acc ++ ["cut forest"]) next

runHttpBatch' acc (Free (Save next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  _ <- post "http://localhost:3000" (pack "save" :: ByteString)
  runHttpBatch' [] next

runHttpBatch' acc (Free (Load next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  resp <- post "http://localhost:3000" (pack "load" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (InspectBranch name next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  let cmd = "inspect branch " ++  name
      rawRequest = cs cmd :: ByteString
  resp <- post "http://localhost:3000" rawRequest
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (InspectBranches name next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  let cmd = "inspect branches " ++  name
      rawRequest = cs cmd :: ByteString
  resp <- post "http://localhost:3000" rawRequest
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (InspectTree name next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  let cmd = "inspect " ++  name
      rawRequest = cs cmd :: ByteString
  resp <- post "http://localhost:3000" rawRequest
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (InspectForest next)) = do
  unless (null acc) $ do
    let batchCommand = unlines acc
        rawRequest = cs batchCommand :: ByteString
    _ <- post "http://localhost:3000" rawRequest
    return ()
  let rawRequest = pack "inspect forest" :: ByteString
  resp <- post "http://localhost:3000" rawRequest
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

-- In-Memory Interpreter
type InMemoryState = Lib2.State

runInMemory :: ForestDSL a -> State InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (PlantTree tree next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.PlantTree tree) of
    Right (_, newState) -> Control.Monad.State.put newState
    Left err -> error err
  runInMemory next
runInMemory (Free (CutBranch name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.CutBranch name) of
    Right (_, newState) -> Control.Monad.State.put newState
    Left err -> error err
  runInMemory next
runInMemory (Free (CutBranches name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.CutBranches name) of
    Right (_, newState) -> Control.Monad.State.put newState
    Left err -> error err
  runInMemory next
runInMemory (Free (CutTree name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.CutTree name) of
    Right (_, newState) -> Control.Monad.State.put newState
    Left err -> error err
  runInMemory next
runInMemory (Free (CutForest next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st Parsers.CutForest of
    Right (_, newState) -> Control.Monad.State.put newState
    Left err -> error err
  runInMemory next
runInMemory (Free (InspectBranch name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.InspectBranch name) of
    Right (Just result, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next result)
    Right (Nothing, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next "")
    Left err -> error err
runInMemory (Free (InspectBranches name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.InspectBranches name) of
    Right (Just result, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next result)
    Right (Nothing, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next "")
    Left err -> error err
runInMemory (Free (InspectTree name next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st (Parsers.InspectTree name) of
    Right (Just result, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next result)
    Right (Nothing, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next "")
    Left err -> error err
runInMemory (Free (InspectForest next)) = do
  st <- Control.Monad.State.get
  case Lib2.stateTransition st Parsers.InspectForest of
    Right (Just result, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next result)
    Right (Nothing, newState) -> do
      Control.Monad.State.put newState
      runInMemory (next "")
    Left err -> error err
runInMemory (Free (Save next)) = do
  -- Saving is a no-op in memory (just keep the current state)
  runInMemory next
runInMemory (Free (Load next)) = do
  -- Loading returns the current state as a string
  currentState <- Control.Monad.State.get
  runInMemory (next $ show currentState)

main :: IO ()
main = do
  args <- getArgs
  let program = do
        load
        plantTree "oak branch leaf"
        plantTree "pine branch leaf leaf"
        inspectForest

  case args of
    ["single"] -> do
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      let (result, finalState) = runState (runInMemory program) Lib2.emptyState
      putStrLn result
      print finalState
    _ -> putStrLn "Usage: stack run forest-client [single|batch|memory]"