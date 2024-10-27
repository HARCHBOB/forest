{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fmax-pmcheck-models=100 #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
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

data Leaves = SingleLeaf Leaf | MultipleLeaves Leaf Leaves
  deriving (Eq, Show)

data Branch = Branch Leaves
  deriving (Eq, Show)

data Branches = SingleBranch Branch | MultipleBranches Branch Branches
  deriving (Eq, Show)

data Tree = Tree Name Branches
  deriving (Eq, Show)

type Forest = [Tree]

type Parser a = String -> Either String (a, String)

-- Parses a specific literal string.
parseLiteral :: String -> Parser String
parseLiteral literal input =
  if take (length literal) input == literal
    then Right (literal, drop (length literal) input)
    else Left $ "Expected: " ++ literal

-- Parses a single character.
parseChar :: Char -> Parser Char
parseChar c (x:xs)
  | c == x = Right (c, xs)
  | otherwise = Left $ "Expected character: " ++ [c]
parseChar _ [] = Left "Unexpected end of input"

-- Parses whitespace (like spaces).
parseWhitespace :: Parser String
parseWhitespace input =
  let (spaces, rest) = span (== ' ') input
   in Right (spaces, rest)

fmap :: (a -> b) -> Parser a -> Parser b
fmap f p input =
  case p input of
    Right (res, rest) -> Right (f res, rest)
    Left err -> Left err

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 input =
  case p1 input of
    Right (res1, rest1) ->
      case p2 rest1 of
        Right (res2, rest2) -> Right (f res1 res2, rest2)
        Left err            -> Left err
    Left err -> Left err

-- Combines three parsers in sequence
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

orN :: [Parser Query] -> Parser Query
orN [] _ = Left "Invalid command"
orN (p:ps) input = case p input of
  Right res -> Right res
  Left _    -> orN ps input

-- Parser for tree names
-- <tree> ::= <name> <branches>
parseName :: Parser Name
parseName input = case words input of
  ("oak":rest)   -> Right (Oak, unwords rest)
  ("pine":rest)  -> Right (Pine, unwords rest)
  ("birch":rest) -> Right (Birch, unwords rest)
  ("marple":rest) -> Right (Marple, unwords rest)
  _              -> Left "Unknown tree name"

-- Leaf parser
-- <leaf> ::= "leaf"
parseLeaf :: Parser Leaf
parseLeaf = Lib2.fmap (const Leaf) (parseLiteral "leaf")

-- Leaves parser
-- <leaves> ::= <leaf> | <leaf> <leaves>
parseLeaves :: Parser Leaves
parseLeaves = orElse parseMultipleLeaves parseSingleLeaf

parseSingleLeaf :: Parser Leaves
parseSingleLeaf = Lib2.fmap SingleLeaf parseLeaf

parseMultipleLeaves :: Parser Leaves
parseMultipleLeaves =
  and2 MultipleLeaves parseLeaf parseLeaves

-- Parses a single branch, which is "branch" followed by leaves
-- <branch> ::= "branch" <leaves>
parseBranch :: Parser Branch
parseBranch = 
  and2 
    (\_ leaves -> Branch leaves) 
    (parseLiteral "branch") 
    parseLeaves

-- Parses either a single branch or multiple branches
-- <branches> ::= <branch> | <branch> <branches>
parseBranches :: Parser Branches
parseBranches =
  orElse parseMultipleBranches parseSingleBranch

-- Parses a single branch (terminal case for branches)
parseSingleBranch :: Parser Branches
parseSingleBranch = Lib2.fmap SingleBranch parseBranch

-- Parses multiple branches recursively
parseMultipleBranches :: Parser Branches
parseMultipleBranches = and2 MultipleBranches parseBranch parseBranches

-- Parser for a tree (a name and its branches)
-- <tree> ::= <name> <branches>
parseTree :: Parser Tree
parseTree input =
  case parseName input of
    Right (name, rest) -> Right (Tree name (SingleBranch (Branch (SingleLeaf Leaf))), rest)
    Left err           -> Left err

-- Plant command parser
parsePlantDefaultTree :: Parser Query
parsePlantDefaultTree =
  and3
    (\_ _ tree -> PlantTree tree)
    (parseLiteral "plant")
    parseWhitespace
    parseTree

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
  and3
    (\_ _ _ -> PlantForest exampleForest)
    (parseLiteral "plant")
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

parseQuery :: String -> Either String Query
parseQuery "" = Left "Command cannot be empty"
parseQuery input =
  case orN [
    parsePlantDefaultTree,
    parsePlantForest,
    parseCutBranch,
    parseCutBranches,
    parseCutTree,
    parseCutForest,
    parseInspectBranch,
    parseInspectBranches,
    parseInspectTree,
    parseInspectForest
  ] input of
  Right (query, _) -> Right query
  Left err         -> Left err

-- Example forest for "plant forest"
exampleForest :: Forest
exampleForest = [Tree Oak (SingleBranch (Branch (SingleLeaf Leaf))), Tree Pine (SingleBranch (Branch (SingleLeaf Leaf)))]

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
    Just (Tree name (SingleBranch (Branch (MultipleLeaves _ ls)))) ->
      let newBranch = Branch ls
          newForest = replaceTree (Tree name (SingleBranch newBranch)) (forest st)
      in Right (Just $ "Cut a branch from " ++ show name, st { forest = newForest })
    Just (Tree name (SingleBranch (Branch (SingleLeaf _)))) ->
      Left $ show name ++ " has no branches to cut."
    Just (Tree name (MultipleBranches _ bs)) ->
      let newForest = replaceTree (Tree name bs) (forest st)
      in Right (Just $ "Cut a branch from " ++ show name, st { forest = newForest })

stateTransition st (CutBranches tName) =
  case findTree tName (forest st) of
    Nothing -> Left ("No such tree: " ++ show tName)
    Just (Tree name (SingleBranch _)) ->
      let newForest = replaceTree (Tree name (SingleBranch (Branch (SingleLeaf Leaf)))) (forest st)
      in Right (Just $ "Cut all branches from " ++ show name, st { forest = newForest })
    Just (Tree name (MultipleBranches _ _)) ->
      let newForest = replaceTree (Tree name (SingleBranch (Branch (SingleLeaf Leaf)))) (forest st)
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
      Right (Just $ "Inspecting a branch of " ++ show name ++ ", leaves: " ++ show (countLeaves leaves), st)
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

countLeaves :: Leaves -> Int
countLeaves (SingleLeaf _) = 1
countLeaves (MultipleLeaves _ ls) = 1 + countLeaves ls
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
