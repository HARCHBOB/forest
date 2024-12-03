{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
module Parsers
    ( Query(..),
    Forest,
    Tree(..),
    Branches(..),
    Branch(..),
    Leaves(..),
    Leaf(..),
    Name(..),
    Parser,
    parse,
    parseLiteral,
    parseChar,
    parseWhitespace,
    and3,
    and5,
    orElse,
    or3,
    or4,
    orN,
    stripPrefix,
    parseName,
    parseLeaf,
    parseLeaves,
    parseSingleLeaf,
    parseMultipleLeaves,
    parseBranch,
    parseBranches,
    parseSingleBranch,
    parseMultipleBranches,
    parseTree,
    parseForest,
    parseCommands
    ) where

import Debug.Trace (trace)
import Control.Applicative
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

--type Parser a = String -> Either String (a, String)

newtype Parser a = P {parse :: String -> Either String (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

-- Example forest for "plant forest"
exampleForest :: Forest
exampleForest = [Tree Oak (SingleBranch (Branch (SingleLeaf Leaf))), Tree Pine (SingleBranch (Branch (SingleLeaf Leaf)))]

parseLiteral :: String -> Parser String
parseLiteral literal = P $ \input ->
  if take (length literal) input == literal
    then Right (literal, drop (length literal) input)
    else Left "Invalid command"

parseChar :: Char -> Parser Char
parseChar c = P $ \input -> case input of
  (x:xs)
    | c == x -> Right (c, xs)
    | otherwise -> Left $ "Expected character: " ++ [c]
  [] -> Left "Unexpected end of input"

parseWhitespace :: Parser String
parseWhitespace = P $ \input ->
  let (spaces, rest) = span (== ' ') input
   in Right (spaces, rest)

fmap :: (a -> b) -> Parser a -> Parser b
fmap f (P p) = P $ \input ->
  case p input of
    Right (res, rest) -> Right (f res, rest)
    Left err -> Left err

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 = P $ \input ->
  case parse p1 input of
    Right (res1, rest1) ->
      case parse p2 rest1 of
        Right (res2, rest2) ->
          case parse p3 rest2 of
            Right (res3, rest3) -> Right (f res1 res2 res3, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f p1 p2 p3 p4 p5 = P $ \input ->
  case parse p1 input of
    Right (res1, rest1) ->
      case parse p2 rest1 of
        Right (res2, rest2) ->
          case parse p3 rest2 of
            Right (res3, rest3) ->
              case parse p4 rest3 of
                Right (res4, rest4) ->
                  case parse p5 rest4 of
                    Right (res5, rest5) -> Right (f res1 res2 res3 res4 res5, rest5)
                    Left err -> Left err
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

orElse :: Parser a -> Parser a -> Parser a
orElse (P p1) (P p2) = P $ \input ->
  case p1 input of
    Right res -> Right res
    Left _    -> p2 input

or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 (P p1) (P p2) (P p3) = P $ \input ->
  case p1 input of
    Right res1 -> Right res1
    Left _ -> case p2 input of
      Right res2 -> Right res2
      Left _ -> p3 input

or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 (P p1) (P p2) (P p3) (P p4) = P $ \input ->
  case p1 input of
    Right res1 -> Right res1
    Left _ -> case p2 input of
      Right res2 -> Right res2
      Left _ -> case p3 input of
        Right res3 -> Right res3
        Left _ -> p4 input

orN :: [Parser Query] -> Parser Query
orN [] = P $ \_ -> Left "Invalid command"
orN (p:ps) = P $ \input -> case parse p input of
  Right res -> Right res
  Left _    -> parse (orN ps) input

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix str
  | take (length prefix) str == prefix = Just (drop (length prefix) str)
  | otherwise                           = Nothing

-- <name> ::= "oak" | "pine" | "birch" | "maple"
parseName :: Parser Name
parseName = P $ \input ->
  if | Just rest <- stripPrefix "oak" input   -> Right (Oak, rest)
     | Just rest <- stripPrefix "pine" input  -> Right (Pine, rest)
     | Just rest <- stripPrefix "birch" input -> Right (Birch, rest)
     | Just rest <- stripPrefix "marple" input -> Right (Marple, rest)
     | otherwise                              -> Left "Unknown tree name"

-- <leaf> ::= "leaf"
parseLeaf :: Parser Leaf
parseLeaf = Parsers.fmap (const Leaf) (parseLiteral "leaf")

-- <leaves> ::= <leaf> | <leaf> <leaves>
parseLeaves :: Parser Leaves
parseLeaves = P $ \input ->
  trace ("Parsed leaves input: " ++ show input) $
  case parse parseMultipleLeaves input of
    Right (leaves, rest) -> Right (leaves, rest)
    Left _               -> parse parseSingleLeaf input

parseSingleLeaf :: Parser Leaves
parseSingleLeaf = P $ \input ->
  case parse parseLeaf input of
    Right (_, rest) -> Right (SingleLeaf Leaf, rest)
    Left err        -> Left err

parseMultipleLeaves :: Parser Leaves
parseMultipleLeaves = P $ \input ->
  case parse parseLeaf input of
    Right (leaf, rest1) ->
      case parse parseWhitespace rest1 of
        Right (_, rest2) ->
          case parse parseLeaves rest2 of
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
parseBranches = P $ \input ->
  trace ("Parsed branches input: " ++ show input) $
  case parse parseMultipleBranches input of
    Right (branches, rest) -> Right (branches, rest)
    Left _                 -> parse parseSingleBranch input

parseSingleBranch :: Parser Branches
parseSingleBranch = P $ \input ->
  case parse parseBranch input of
    Right (branch, rest) -> Right (SingleBranch branch, rest)
    Left err             -> Left err

parseMultipleBranches :: Parser Branches
parseMultipleBranches = P $ \input ->
  case parse parseBranch input of
    Right (branch, rest1) ->
      case parse parseWhitespace rest1 of
        Right (_, rest2) ->
          case parse parseBranches rest2 of
            Right (branches, rest3) -> Right (MultipleBranches branch branches, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- <tree> ::= <name> <branches>
parseTree :: Parser Tree
parseTree = P $ \input ->
  trace ("Parsed tree input: " ++ show input) $
  case parse parseName input of
    Right (name, rest1) ->
      case parse parseWhitespace rest1 of
        Right (_, rest2) ->
          case parse parseBranches rest2 of
            Right (branches, rest3) -> Right (Tree name branches, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- <forest> ::= <tree> | <tree> <forest>
parseForest :: Parser Forest
parseForest = P $ \input ->
  trace ("Parsed forest input: " ++ show input) $
  case parse parseTree input of
    Right (tree, rest1) ->
      case parse parseWhitespace rest1 of
        Right (_, rest2) ->
          case parse parseForest rest2 of
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
