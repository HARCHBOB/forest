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
    parseCommands
    ) where

import Control.Applicative
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf1, suchThat, oneof)
--import Debug.Trace (trace)
--trace ("Parsed name: " ++ show name ++ ", Remaining: " ++ show rest1) $

data Query
  = PlantTree Tree
  | CutBranch Name
  | CutBranches Name
  | CutTree Name
  | CutForest
  | InspectBranch Name
  | InspectBranches Name
  | InspectTree Name
  | InspectForest
  deriving (Eq, Show)

data Name = Oak | Pine | Birch | Maple
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


stripPrefix :: String -> String -> Maybe String
stripPrefix prefix str
  | take (length prefix) str == prefix = Just (drop (length prefix) str)
  | otherwise                           = Nothing

-- <name> ::= "oak" | "pine" | "birch" | "maple"
parseName :: Parser Name
parseName = 
  (parseLiteral "oak" >> return Oak) <|>
  (parseLiteral "pine" >> return Pine) <|>
  (parseLiteral "birch" >> return Birch) <|>
  (parseLiteral "maple" >> return Maple)

-- <leaf> ::= "leaf"
parseLeaf :: Parser Leaf
parseLeaf = Parsers.fmap (const Leaf) (parseLiteral "leaf")

-- <leaves> ::= <leaf> | <leaf> <leaves>
parseLeaves :: Parser Leaves
parseLeaves = parseMultipleLeaves <|> parseSingleLeaf

parseSingleLeaf :: Parser Leaves
parseSingleLeaf = do
  leaf <- parseLeaf
  return $ SingleLeaf leaf
    
parseMultipleLeaves :: Parser Leaves
parseMultipleLeaves = do
  leaf <- parseLeaf
  _ <- parseWhitespace
  leaves <- parseLeaves
  return $ MultipleLeaves leaf leaves

-- <branch> ::= "branch" <leaves>
parseBranch :: Parser Branch
parseBranch = do
  _ <- parseLiteral "branch"
  _ <- parseWhitespace
  leaves <- parseLeaves
  return $ Branch leaves

-- <branches> ::= <branch> | <branch> <branches>
parseBranches :: Parser Branches
parseBranches = parseMultipleBranches <|> parseSingleBranch <|> return None

parseSingleBranch :: Parser Branches
parseSingleBranch = do
  branch <- parseBranch
  return $ SingleBranch branch

parseMultipleBranches :: Parser Branches
parseMultipleBranches = do
  branch <- parseBranch
  _ <- parseWhitespace
  branches <- parseBranches
  return $ MultipleBranches branch branches

-- <tree> ::= <name> <branches>
parseTree :: Parser Tree
parseTree = do
  name <- parseName
  _ <- parseWhitespace
  branches <- parseBranches
  return $ Tree name branches

parsePlantTree :: Parser Query
parsePlantTree = do
  _ <- parseLiteral "plant"
  _ <- parseWhitespace
  tree <- parseTree
  return $ PlantTree tree

parseCutBranch :: Parser Query
parseCutBranch = do
  _ <- parseLiteral "cut"
  _ <- parseWhitespace
  _ <- parseLiteral "branch"
  _ <- parseWhitespace
  name <- parseName
  return $ CutBranch name

parseCutBranches :: Parser Query
parseCutBranches = do
  _ <- parseLiteral "cut"
  _ <- parseWhitespace
  _ <- parseLiteral "branches"
  _ <- parseWhitespace
  name <- parseName
  return $ CutBranches name

parseCutTree :: Parser Query
parseCutTree = do
  _ <- parseLiteral "cut"
  _ <- parseWhitespace
  name <- parseName
  return $ CutTree name

parseCutForest :: Parser Query
parseCutForest = do
  _ <- parseLiteral "cut"
  _ <- parseWhitespace
  _ <- parseLiteral "forest"
  return CutForest

parseInspectBranch :: Parser Query
parseInspectBranch = do
  _ <- parseLiteral "inspect"
  _ <- parseWhitespace
  _ <- parseLiteral "branch"
  _ <- parseWhitespace
  name <- parseName
  return $ InspectBranch name

parseInspectBranches :: Parser Query
parseInspectBranches = do
  _ <- parseLiteral "inspect"
  _ <- parseWhitespace
  _ <- parseLiteral "branches"
  _ <- parseWhitespace
  name <- parseName
  return $ InspectBranches name

parseInspectTree :: Parser Query
parseInspectTree = do
  _ <- parseLiteral "inspect"
  _ <- parseWhitespace
  name <- parseName
  return $ InspectTree name

parseInspectForest :: Parser Query
parseInspectForest = do
  _ <- parseLiteral "inspect"
  _ <- parseWhitespace
  _ <- parseLiteral "forest"
  return InspectForest

parsePlantCommand :: Parser Query
parsePlantCommand = parsePlantTree

parseCutCommand :: Parser Query
parseCutCommand = 
  parseCutBranch <|> 
  parseCutBranches <|> 
  parseCutTree <|> 
  parseCutForest

parseInspectCommand :: Parser Query
parseInspectCommand = 
  parseInspectBranch <|> 
  parseInspectBranches <|> 
  parseInspectTree <|> 
  parseInspectForest

parseCommands :: Parser Query
parseCommands =
    parsePlantCommand <|> 
    parseCutCommand <|> 
    parseInspectCommand



instance Arbitrary Query where
  arbitrary =
    oneof
      [ PlantTree <$> arbitrary,
        CutBranch <$> arbitrary,
        CutBranches <$> arbitrary,
        CutTree <$> arbitrary,
        pure CutForest,
        InspectBranch <$> arbitrary,
        InspectBranches <$> arbitrary,
        InspectTree <$> arbitrary,
        pure InspectForest
      ]

instance Arbitrary Name where
  arbitrary = elements [Oak, Pine, Birch, Maple]

instance Arbitrary Leaf where
  arbitrary = pure Leaf

instance Arbitrary Leaves where
  arbitrary =
    oneof
      [ SingleLeaf <$> arbitrary,
        MultipleLeaves <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Branch where
  arbitrary = Branch <$> arbitrary

instance Arbitrary Branches where
  arbitrary =
    oneof
      [ pure None,
        SingleBranch <$> arbitrary,
        MultipleBranches <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Tree where
  arbitrary = Tree <$> arbitrary <*> arbitrary