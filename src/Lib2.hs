{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    main
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

-- Defined constructors for the plant, cut, and inspect commands.
data Query
  = PlantTree Tree
  | PlantForest Forest
  | CutBranch Name
  | CutTree Name
  | CutForest
  | InspectBranch Name
  | InspectTree Name
  | InspectForest
  deriving (Eq, Show)

-- | The 'Name' of a tree, following the BNF.
data Name = Oak | Pine | Birch | Marple
  deriving (Eq, Show)

-- | The structure of forest.
data Leaf = Leaf
  deriving (Eq, Show)

data Branch = Branch[Leaf]
  deriving(Eq, Show)

data Tree = Tree Name [Branch]
  deriving (Eq, Show)

type Forest = [Tree]


-- | The instances are needed basically for tests
--instance Eq Query where
--  (==) _ _= False

--instance Show Query where
--  show _ = ""


-- | Parses user's input.
-- The function must have tests.

-- Parses strings like "plant oak", "cut forest", ect.
parseQuery :: String -> Either String Query
parseQuery input = case words input of 
  ["plant", "oak"]                -> Right $ PlantTree (Tree Oak [Branch [Leaf]])
  ["plant", "pine"]               -> Right $ PlantTree (Tree Pine [Branch [Leaf]])
  ["plant", "birch"]              -> Right $ PlantTree (Tree Birch [Branch [Leaf]])
  ["plant", "marple"]             -> Right $ PlantTree (Tree Marple [Branch [Leaf]])
  ["plant", "forest"]             -> Right $ PlantForest exampleForest
  ["cut", "branch", "oak"]        -> Right $ CutBranch Oak
  ["cut", "branch", "pine"]       -> Right $ CutBranch Pine
  ["cut", "branch", "birch"]      -> Right $ CutBranch Birch
  ["cut", "branch", "marple"]     -> Right $ CutBranch Marple
  ["cut", "oak"]                  -> Right $ CutTree Oak
  ["cut", "pine"]                 -> Right $ CutTree Pine
  ["cut", "birch"]                -> Right $ CutTree Birch
  ["cut", "marple"]               -> Right $ CutTree Marple
  ["cut", "forest"]               -> Right CutForest
  ["inspect", "branch", "oak"]    -> Right $ InspectBranch Oak
  ["inspect", "branch", "pine"]   -> Right $ InspectBranch Pine
  ["inspect", "branch", "birch"]  -> Right $ InspectBranch Birch
  ["inspect", "branch", "marple"] -> Right $ InspectBranch Marple
  ["inspect", "oak"]              -> Right $ InspectTree Oak
  ["inspect", "pine"]             -> Right $ InspectTree Pine
  ["inspect", "birch"]            -> Right $ InspectTree Birch
  ["inspect", "marple"]           -> Right $ InspectTree Marple
  ["inspect", "forest"]           -> Right InspectForest
  _                               -> Left "Invalid command"

-- Example forest for parsing "plant forest"
exampleForest :: Forest
exampleForest = [Tree Oak [Branch [Leaf]], Tree Pine [Branch [Leaf]]]

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.

-- The forest changes based on the commands.
data State = State { forest :: Forest }
  deriving (Show, Eq)

-- | Creates an initial program's state.
-- It is called once when the program starts.

-- Creates an initial program state with an empty forest.
emptyState :: State
emptyState = State { forest = [] }

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.

-- Handles state transitions based on a given query.
-- Updates the state and provides an optional message to print.
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
    Just (Tree name (b:bs)) -> 
      let newForest = replaceTree (Tree name bs) (forest st)
      in Right (Just $ "Cut a branch from " ++ show name, st { forest = newForest })
    Just (Tree name []) -> Left $ show name ++ "has no branches to cut."

stateTransition st (CutTree name) =
  let newForest = filter (\(Tree n _) -> n /= name) (forest st)
  in Right (Just ("Cut down the " ++ show name ++ " tree."), st { forest = newForest })

stateTransition st CutForest = 
  Right (Just "Cut down the entire forest.", st { forest = [] })

stateTransition st (InspectBranch tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (b:_)) -> Right (Just $ "Inspecting branch of " ++ show name, st)
    Just (Tree name []) -> Left $ show name ++ " has no branches to inspect."

stateTransition st (InspectTree name) =
  case findTree name (forest st) of
    Just tree -> Right (Just $ "Inspecting tree: " ++ showTreeName tree, st)
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
  "Tree: " ++ show name ++
  "\n  Number of branches: " ++ show (length branches) ++
  "\n  " ++ unlines (map showBranchDetails branches)

showBranchDetails :: Branch -> String
showBranchDetails (Branch leaves) =
  "Branch with " ++ show (length leaves) ++ " leaves"

showTreeName :: Tree -> String
showTreeName (Tree name _) = show name

treeName :: Tree -> Name
treeName (Tree name _) = name

replaceTree :: Tree -> Forest -> Forest
replaceTree newTree = map (\t -> if treeName t == treeName newTree then newTree else t)

findTree :: Name -> Forest -> Maybe Tree
findTree _ [] = Nothing
findTree name (t@(Tree n _):ts)
  | n == name = Just t 
  | otherwise = findTree name ts



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