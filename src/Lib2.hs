{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fmax-pmcheck-models=100 #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

--import Debug.Trace (trace)
--trace ("Parsed name: " ++ show name ++ ", Remaining: " ++ show rest1) $

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

data Name = Oak | Pine | Birch | Marple
  deriving (Eq, Show)

data Leaf = Leaf
  deriving (Eq, Show)

data Leaves = SingleLeaf Leaf | MultipleLeaves Leaf Leaves
  deriving (Eq, Show)

data Branch = Branch Leaves
  deriving (Eq, Show)

data Branches = SingleBranch Branch | MultipleBranches Branch Branches | None
  deriving (Eq, Show)

data Tree = Tree Name Branches
  deriving (Eq, Show)

type Forest = [Tree]

type Parser a = String -> Either String (a, String)

parseLiteral :: String -> Parser String
parseLiteral literal input =
  if take (length literal) input == literal
    then Right (literal, drop (length literal) input)
    else Left $ "Invalid command"

parseChar :: Char -> Parser Char
parseChar c (x:xs)
  | c == x = Right (c, xs)
  | otherwise = Left $ "Expected character: " ++ [c]
parseChar _ [] = Left "Unexpected end of input"

parseWhitespace :: Parser String
parseWhitespace input =
  let (spaces, rest) = span (== ' ') input
   in Right (spaces, rest)

fmap :: (a -> b) -> Parser a -> Parser b
fmap f p input =
  case p input of
    Right (res, rest) -> Right (f res, rest)
    Left err -> Left err

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 input =
  case p1 input of
    Right (res1, rest1) ->
      case p2 rest1 of
        Right (res2, rest2) ->
          case p3 rest2 of
            Right (res3, rest3) -> Right (f res1 res2 res3, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f p1 p2 p3 p4 p5 input =
  case p1 input of
    Right (res1, rest1) ->
      case p2 rest1 of
        Right (res2, rest2) ->
          case p3 rest2 of
            Right (res3, rest3) ->
              case p4 rest3 of
                Right (res4, rest4) ->
                  case p5 rest4 of
                    Right (res5, rest5) -> Right (f res1 res2 res3 res4 res5, rest5)
                    Left err -> Left err
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input =
  case p1 input of
    Right res -> Right res
    Left _    -> p2 input

or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 p1 p2 p3 input = 
  case p1 input of
    Right res1 -> Right res1
    Left _ -> case p2 input of
      Right res2 -> Right res2
      Left _ -> p3 input

or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 p1 p2 p3 p4 input = 
  case p1 input of
    Right res1 -> Right res1
    Left _ -> case p2 input of
      Right res2 -> Right res2
      Left _ -> case p3 input of
        Right res3 -> Right res3
        Left _ -> p4 input

orN :: [Parser Query] -> Parser Query
orN [] _ = Left "Invalid command"
orN (p:ps) input = case p input of
  Right res -> Right res
  Left _    -> orN ps input

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix str
  | take (length prefix) str == prefix = Just (drop (length prefix) str)
  | otherwise                           = Nothing

-- <name> ::= "oak" | "pine" | "birch" | "maple"
parseName :: Parser Name
parseName input
  | Just rest <- stripPrefix "oak" input   = Right (Oak, rest)
  | Just rest <- stripPrefix "pine" input  = Right (Pine, rest)
  | Just rest <- stripPrefix "birch" input = Right (Birch, rest)
  | Just rest <- stripPrefix "marple" input = Right (Marple, rest)
  | otherwise                              = Left "Unknown tree name"

-- <leaf> ::= "leaf"
parseLeaf :: Parser Leaf
parseLeaf = Lib2.fmap (const Leaf) (parseLiteral "leaf")

-- <leaves> ::= <leaf> | <leaf> <leaves>
parseLeaves :: Parser Leaves
parseLeaves input =
  case parseMultipleLeaves input of
    Right (leaves, rest) -> Right (leaves, rest)
    Left _               -> parseSingleLeaf input

parseSingleLeaf :: Parser Leaves
parseSingleLeaf input =
  case parseLeaf input of
    Right (_, rest) -> Right (SingleLeaf Leaf, rest)
    Left err        -> Left err

parseMultipleLeaves :: Parser Leaves
parseMultipleLeaves input =
  case parseLeaf input of
    Right (leaf, rest1) ->
      case parseWhitespace rest1 of
        Right (_, rest2) ->
          case parseLeaves rest2 of
            Right (leaves, rest3) -> Right (MultipleLeaves leaf leaves, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- <branch> ::= "branch" <leaves>
parseBranch :: Parser Branch
parseBranch =
  and3
    (\_ _ leaves -> Branch leaves)
    (parseLiteral "branch")
    parseWhitespace
    parseLeaves

-- <branches> ::= <branch> | <branch> <branches>
parseBranches :: Parser Branches
parseBranches input =
  case parseMultipleBranches input of
    Right (branches, rest) -> Right (branches, rest)
    Left _                  -> parseSingleBranch input

parseSingleBranch :: Parser Branches
parseSingleBranch input =
  case parseBranch input of
    Right (branch, rest) -> Right (SingleBranch branch, rest)
    Left err             -> Left err

parseMultipleBranches :: Parser Branches
parseMultipleBranches input =
  case parseBranch input of
    Right (branch, rest1) ->
      case parseWhitespace rest1 of
        Right (_, rest2) ->
          case parseBranches rest2 of
            Right (branches, rest3) -> Right (MultipleBranches branch branches, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- <tree> ::= <name> <branches>
parseTree :: Parser Tree
parseTree input =
  case parseName input of
    Right (name, rest1) ->
      case parseWhitespace rest1 of
        Right (_, rest2) ->
          case parseBranches rest2 of
            Right (branches, rest3) -> Right (Tree name branches, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- <forest> ::= <tree> | <tree> <forest>
parseForest :: Parser Forest
parseForest input =
  case parseTree input of
    Right (tree, rest1) ->
      case parseWhitespace rest1 of
        Right (_, rest2) ->
          case parseForest rest2 of
            Right (forest1, rest3) ->
              Right (tree : forest1, rest3)  -- Combine tree with the parsed forest
            Left _ -> Right ([tree], rest2) -- No more trees, return single tree as forest
        Left err -> Left err
    Left err -> Left err


-- Plant command parser
parsePlantTree :: Parser Query
parsePlantTree =
  and3
    (\_ _ tree -> PlantTree tree)
    (parseLiteral "plant")
    parseWhitespace
    parseTree

-- Plant forest parser
parsePlantForest :: Parser Query
parsePlantForest =
  and5
    (\_ _ _ _ forest1 -> PlantForest forest1)
    (parseLiteral "plant")
    parseWhitespace
    (parseLiteral "forest")
    parseWhitespace
    parseForest

parsePlantExampleForest :: Parser Query
parsePlantExampleForest =
  and5
    (\_ _ _ _ _ -> PlantForest exampleForest)
    (parseLiteral "plant")
    parseWhitespace
    (parseLiteral "example")
    parseWhitespace
    (parseLiteral "forest")

-- Cut command parser
parseCutBranch :: Parser Query
parseCutBranch =
  and5
    (\_ _ _ _ name -> CutBranch name)
    (parseLiteral "cut")
    parseWhitespace
    (parseLiteral "branch")
    parseWhitespace
    parseName

parseCutBranches :: Parser Query
parseCutBranches =
  and5
    (\_ _ _ _ name -> CutBranches name)
    (parseLiteral "cut")
    parseWhitespace
    (parseLiteral "branches")
    parseWhitespace
    parseName

parseCutTree :: Parser Query
parseCutTree =
  and3
    (\_ _ name -> CutTree name)
    (parseLiteral "cut")
    parseWhitespace
    parseName

parseCutForest :: Parser Query
parseCutForest =
  and3
    (\_ _ _ -> CutForest)
    (parseLiteral "cut")
    parseWhitespace
    (parseLiteral "forest")

-- Inspect command parser
parseInspectBranch :: Parser Query
parseInspectBranch =
  and5
    (\_ _ _ _ name -> InspectBranch name)
    (parseLiteral "inspect")
    parseWhitespace
    (parseLiteral "branch")
    parseWhitespace
    parseName

parseInspectBranches :: Parser Query
parseInspectBranches =
  and5
    (\_ _ _ _ name -> InspectBranches name)
    (parseLiteral "inspect")
    parseWhitespace
    (parseLiteral "branches")
    parseWhitespace
    parseName

parseInspectTree :: Parser Query
parseInspectTree =
  and3
    (\_ _ name -> InspectTree name)
    (parseLiteral "inspect")
    parseWhitespace
    parseName

parseInspectForest :: Parser Query
parseInspectForest =
  and3
    (\_ _ _ -> InspectForest)
    (parseLiteral "inspect")
    parseWhitespace
    (parseLiteral "forest")

parsePlantCommand :: Parser Query
parsePlantCommand = 
  or3
    parsePlantTree
    parsePlantForest
    parsePlantExampleForest

parseCutCommand :: Parser Query
parseCutCommand = 
  or4
    parseCutBranch
    parseCutBranches
    parseCutTree
    parseCutForest

parseInspectCommand :: Parser Query
parseInspectCommand =
  or4
    parseInspectBranch
    parseInspectBranches
    parseInspectTree
    parseInspectForest

parseCommands :: Parser Query
parseCommands = 
  or3
    parsePlantCommand
    parseCutCommand
    parseInspectCommand

parseQuery :: String -> Either String Query
parseQuery "" = Left "Command cannot be empty"
parseQuery input =
  case parseCommands input of
  Right (query, _) -> Right query
  Left err         -> Left err

-- Example forest for "plant forest"
exampleForest :: Forest
exampleForest = [Tree Oak (SingleBranch (Branch (SingleLeaf Leaf))), Tree Pine (SingleBranch (Branch (SingleLeaf Leaf)))]

data State = State { forest :: Forest }
  deriving (Show, Eq)

-- | Initial state with an empty forest.
emptyState :: State
emptyState = State { forest = [] }


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
    Just (Tree name None) ->
      Right (Just $ "No branches to cut on " ++ show name, st)
    Just (Tree name (SingleBranch _)) ->
      let newForest = replaceTree (Tree name None) (forest st)
      in Right (Just $ "Cut the only branch from " ++ show name, st { forest = newForest })
    Just (Tree name (MultipleBranches _ remainingBranches)) ->
      let newForest = replaceTree (Tree name remainingBranches) (forest st)
      in Right (Just $ "Cut the first branch from " ++ show name, st { forest = newForest })

stateTransition st (CutBranches tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name None) ->
      Right (Just $ "No branches to cut on " ++ show name, st)
    Just (Tree name _) ->
      let newForest = replaceTree (Tree name None) (forest st)
      in Right (Just $ "Cut all branches from " ++ show name, st { forest = newForest })

stateTransition st (CutTree name) =
  let newForest = filter (\(Tree n _) -> n /= name) (forest st)
  in Right (Just ("Cut down the " ++ show name ++ " tree."), st { forest = newForest })

stateTransition st CutForest =
  Right (Just "Cut down the entire forest.", st { forest = [] })

stateTransition st (InspectBranch tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name None) ->
      Right (Just $ "No branches on " ++ show name, st)
    Just (Tree name (SingleBranch branch)) ->
      Right (Just $ "Single branch found on " ++ show name ++ ": " ++ show branch, st)
    Just (Tree name (MultipleBranches branch _)) ->
      Right (Just $ "First branch found on " ++ show name ++ ": " ++ show branch, st)

stateTransition st (InspectBranches tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name None) ->
      Right (Just $ "No branches on " ++ show name, st)
    Just (Tree name (SingleBranch branch)) ->
      Right (Just $ "One branch on " ++ show name ++ ": " ++ show branch, st)
    Just (Tree name (MultipleBranches branch branches)) ->
      Right (Just $ "Multiple branches on " ++ show name ++ ": " ++ showAllBranches branch branches, st)

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

countLeaves :: Leaves -> Int
countLeaves (SingleLeaf _) = 1
countLeaves (MultipleLeaves _ ls) = 1 + countLeaves ls

showAllBranches :: Branch -> Branches -> String
showAllBranches branch None = show branch
showAllBranches branch (SingleBranch b) = show branch ++ ", " ++ show b
showAllBranches branch (MultipleBranches b bs) = show branch ++ ", " ++ showAllBranches b bs

countBranches :: Branches -> Int
countBranches None = 0
countBranches (SingleBranch _) = 1
countBranches (MultipleBranches _ bs) = 1 + countBranches bs

showTreeName :: Tree -> String
showTreeName (Tree name _) = show name

getTreeName :: Tree -> Name
getTreeName (Tree name _) = name

replaceTree :: Tree -> Forest -> Forest
replaceTree newTree = map (\t -> if getTreeName t == getTreeName newTree then newTree else t)

findTree :: Name -> Forest -> Maybe Tree
findTree _ [] = Nothing
findTree name (t@(Tree n _):ts)
  | n == name = Just t
  | otherwise = findTree name ts
