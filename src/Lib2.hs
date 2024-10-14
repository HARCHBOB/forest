{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    main
    ) where

-- | Command representation based on the BNF grammar.
data Query
  = PlantTree Tree
  | PlantForest Forest
  | CutBranch Name
  | CutBranches Name
  | CutTree Name
  | CutForest
  | InspectBranch Name
  | InspectBranches Name
  | InspectTree Name
  | InspectForest
  deriving (Eq, Show)

-- | The 'Name' of a tree, following the BNF.
data Name = Oak | Pine | Birch | Marple
  deriving (Eq, Show)

-- | Recursive structure of leaves and branches.
data Leaf = Leaf
  deriving (Eq, Show)

data Branch = Branch [Leaf]
  deriving (Eq, Show)

data Branches = SingleBranch Branch | MultipleBranches Branch Branches
  deriving (Eq, Show)

data Leaves = SingleLeaf Leaf | MultipleLeaves Leaf Leaves
  deriving (Eq, Show)

data Tree = Tree Name Branches
  deriving (Eq, Show)

type Forest = [Tree]

-- | Parses user input to match the BNF grammar.
parseQuery :: String -> Either String Query
parseQuery "" = Left "Command cannot be empty"  -- Updated error message for empty input
parseQuery input = case words input of 
  ["plant", "oak"]                -> Right $ PlantTree (Tree Oak (SingleBranch (Branch [Leaf])))
  ["plant", "pine"]               -> Right $ PlantTree (Tree Pine (SingleBranch (Branch [Leaf])))
  ["plant", "birch"]              -> Right $ PlantTree (Tree Birch (SingleBranch (Branch [Leaf])))
  ["plant", "marple"]             -> Right $ PlantTree (Tree Marple (SingleBranch (Branch [Leaf])))
  ["plant", "forest"]             -> Right $ PlantForest exampleForest
  ["cut", "branch", treeName]     -> parseCutBranch treeName
  ["cut", "branches", treeName]   -> parseCutBranches treeName
  ["cut", treeName]               -> parseCutTree treeName
  ["cut", "forest"]               -> Right CutForest
  ["inspect", "branch", treeName] -> parseInspectBranch treeName
  ["inspect", "branches", treeName] -> parseInspectBranches treeName
  ["inspect", "forest"]           -> Right InspectForest
  ["inspect", treeName]           -> parseInspectTree treeName
  _                               -> Left "Invalid command"

-- Example forest for "plant forest"
exampleForest :: Forest
exampleForest = [Tree Oak (SingleBranch (Branch [Leaf])), Tree Pine (SingleBranch (Branch [Leaf]))]

-- | An entity which represents your program's state.
data State = State { forest :: Forest }
  deriving (Show, Eq)

-- | Initial state with an empty forest.
emptyState :: State
emptyState = State { forest = [] }

-- | Handles state transitions based on a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st (PlantTree tree) =
  let newForest = tree : forest st
  in Right (Just ("Planted a new " ++ showTreeName tree), st { forest = newForest })

stateTransition st (PlantForest newForest) =
  let updatedForest = newForest ++ forest st
  in Right (Just "Planted a new forest.", st { forest = updatedForest })

stateTransition st (CutBranch tName) = 
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (SingleBranch (Branch (l:ls)))) -> 
      let newBranch = Branch ls
          newForest = replaceTree (Tree name (SingleBranch newBranch)) (forest st)
      in Right (Just $ "Cut a branch from " ++ show name, st { forest = newForest })
    Just (Tree name (MultipleBranches _ bs)) -> 
      let newForest = replaceTree (Tree name bs) (forest st)
      in Right (Just $ "Cut a branch from " ++ show name, st { forest = newForest })
    Just (Tree name _) -> Left $ show name ++ " has no branches to cut."

stateTransition st (CutBranches tName) = 
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (SingleBranch _)) -> 
      let newForest = replaceTree (Tree name (SingleBranch (Branch []))) (forest st)
      in Right (Just $ "Cut all branches from " ++ show name, st { forest = newForest })
    Just (Tree name (MultipleBranches _ _)) -> 
      let newForest = replaceTree (Tree name (SingleBranch (Branch []))) (forest st)
      in Right (Just $ "Cut all branches from " ++ show name, st { forest = newForest })

stateTransition st (CutTree name) =
  let newForest = filter (\(Tree n _) -> n /= name) (forest st)
  in Right (Just ("Cut down the " ++ show name ++ " tree."), st { forest = newForest })

stateTransition st CutForest = 
  Right (Just "Cut down the entire forest.", st { forest = [] })

stateTransition st (InspectBranch tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (SingleBranch (Branch leaves))) -> 
      Right (Just $ "Inspecting a branch of " ++ show name ++ ", leaves: " ++ show (length leaves), st)
    Just (Tree name (MultipleBranches _ _)) -> 
      Right (Just $ "Inspecting branches of " ++ show name, st)

stateTransition st (InspectBranches tName) = 
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (MultipleBranches _ _)) -> 
      Right (Just $ "Inspecting all branches of " ++ show name, st)
    Just (Tree name (SingleBranch _)) -> 
      Right (Just $ "Inspecting a single branch of " ++ show name, st)

stateTransition st (InspectTree name) =
  case findTree name (forest st) of
    Just tree -> Right (Just $ "Inspecting tree:\n" ++ showTreeDetails tree, st)
    Nothing -> Left ("No such tree: " ++ show name)

stateTransition st InspectForest = 
  if null (forest st)
    then Right (Just "The forest is empty.", st)
    else 
      let detailedInfo = unlines (map showTreeDetails (forest st))
      in Right (Just $ "Forest details:\n" ++ detailedInfo, st)

-- | Helper functions
showTreeDetails :: Tree -> String
showTreeDetails (Tree name branches) =
  "  Tree: " ++ show name ++
  "\n  Number of branches: " ++ show (countBranches branches)

countBranches :: Branches -> Int
countBranches (SingleBranch _) = 1
countBranches (MultipleBranches _ bs) = 1 + countBranches bs

showTreeName :: Tree -> String
showTreeName (Tree name _) = show name

-- Renamed treeName to getTreeName to avoid name conflict
getTreeName :: Tree -> Name
getTreeName (Tree name _) = name

replaceTree :: Tree -> Forest -> Forest
replaceTree newTree = map (\t -> if getTreeName t == getTreeName newTree then newTree else t)

findTree :: Name -> Forest -> Maybe Tree
findTree _ [] = Nothing
findTree name (t@(Tree n _):ts)
  | n == name = Just t 
  | otherwise = findTree name ts

parseCutBranch, parseCutBranches, parseCutTree, parseInspectBranch, parseInspectBranches, parseInspectTree :: String -> Either String Query
parseCutBranch = mapTreeCommand CutBranch
parseCutBranches = mapTreeCommand CutBranches
parseCutTree = mapTreeCommand CutTree
parseInspectBranch = mapTreeCommand InspectBranch
parseInspectBranches = mapTreeCommand InspectBranches
parseInspectTree = mapTreeCommand InspectTree

mapTreeCommand :: (Name -> Query) -> String -> Either String Query
mapTreeCommand cmd treeName = case treeName of
  "oak"   -> Right $ cmd Oak
  "pine"  -> Right $ cmd Pine
  "birch" -> Right $ cmd Birch
  "marple" -> Right $ cmd Marple
  _       -> Left "Unknown tree name"

-- Main REPL function
main :: IO ()
main = repl emptyState

-- | REPL loop that takes the current state and processes user commands
repl :: State -> IO ()
repl state = do
    putStrLn "Enter a command ('exit' to quit):"
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else case parseQuery input of
            Left err -> do
                putStrLn $ "Error: " ++ err
                repl state
            Right query -> do
                case stateTransition state query of
                    Left err -> do
                        putStrLn $ "Error: " ++ err
                        repl state
                    Right (msg, newState) -> do
                        case msg of
                            Just message -> putStrLn message
                            Nothing -> return ()
                        repl newState
