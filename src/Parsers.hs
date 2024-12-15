{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
    try,
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

import Control.Applicative (Alternative (..), optional, (<|>))
import Test.QuickCheck (Arbitrary (..), elements, oneof)
import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Char (isSpace, toLower)
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

newtype Parser a = Parser { runParser :: ExceptT String (State String) a }
  deriving (Functor, Applicative, Monad, MonadError String)

-- Custom Alternative instance
instance Alternative Parser where
  empty = throwError "empty"
  p1 <|> p2 = p1 `catchError` (\_ -> p2)

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT (runParser parser))

try :: Parser a -> Parser a
try p = Parser $ do
    stateBefore <- lift get
    runParser p `catchError` \e -> do
        lift (put stateBefore)
        throwError e

-- skipSpaces' :: Parser ()
-- skipSpaces' = lift (modify (dropWhile (== ' ')))

-- parseLiteral :: String -> Parser String
-- parseLiteral [] = return []
-- parseLiteral (x : xs) = do
--   skipSpaces'
--   _ <- parseChar x
--   --trace ("Parsed literal: " ++ [x]) (parseLiteral xs)

skipSpaces :: Parser ()
skipSpaces = Parser $ do
    input <- lift get
    let rest = dropWhile isSpace input
    lift (put rest)

parseLiteral :: String -> Parser String
parseLiteral s = Parser $ do
    runParser skipSpaces
    input <- lift get
    ----trace ("parseLiteral expecting \"" ++ s ++ "\" got: " ++ show (take 25 input) ++ "...") (return ())
    let len = length s
        (prefix, rest) = splitAt len input
    if map toLower prefix == map toLower s
       then do
           lift (put rest)
           return s
       else throwError $ "Expected \"" ++ s ++ "\", but found \"" ++ prefix ++ "\""

sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ do
  input <- lift get
  --trace ("Current input: " ++ input) $
  case input of
    [] -> throwError "Empty String"
    (x : xs) ->
      if p x
        then lift (put xs) >> return x
        else throwError $ "Could not recognize: " ++ [x]

parseChar :: Char -> Parser Char
parseChar c = sat (== c)

parseWhitespace :: Parser String
parseWhitespace = Parser $ do
  input <- lift get
  let (spaces, rest) = span (== ' ') input
  lift (put rest)
  return spaces
  --trace ("Parsed whitespace: " ++ spaces) (return spaces)

fmap :: (a -> b) -> Parser a -> Parser b
fmap f p = do
  result <- p
  return (f result)

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
parseLeaf = do
  _ <- parseLiteral "leaf"
  return Leaf
  --trace "Parsed leaf" (return Leaf)

-- <leaves> ::= <leaf> | <leaf> <leaves>
parseLeaves :: Parser Leaves
parseLeaves = 
  try parseMultipleLeaves <|> 
  parseSingleLeaf

parseSingleLeaf :: Parser Leaves
parseSingleLeaf = do
  leaf <- parseLeaf
  return $ SingleLeaf leaf
  --trace ("Parsed single leaf: " ++ show leaf) (return $ SingleLeaf leaf)

parseMultipleLeaves :: Parser Leaves
parseMultipleLeaves = do
  leaf <- parseLeaf
  --trace ("Parsed leaf in multiple leaves: " ++ show leaf) (return ())
  leaves <- parseLeaves
  return $ MultipleLeaves leaf leaves
  --trace ("Parsed multiple leaves: " ++ show leaf ++ ", " ++ show leaves) (return $ MultipleLeaves leaf leaves)

-- <branch> ::= "branch" <leaves>
parseBranch :: Parser Branch
parseBranch = do
  _ <- parseLiteral "branch"
  --trace ("Parsed branch") (return ())
  leaves <- parseLeaves
  return $ Branch leaves
  --trace ("Parsed branch with leaves: " ++ show leaves) (return $ Branch leaves)

-- <branches> ::= <branch> | <branch> <branches>
parseBranches :: Parser Branches
parseBranches = 
  try parseMultipleBranches <|> 
  try parseSingleBranch <|> 
  parseNoneBranches

parseNoneBranches :: Parser Branches
parseNoneBranches = do
  _ <- parseLiteral "none"
  return None
  --trace "Parsed none branches" (return None)

parseSingleBranch :: Parser Branches
parseSingleBranch = do
  branch <- parseBranch
  return $ SingleBranch branch
  --trace ("Parsed single branch: " ++ show branch) (return $ SingleBranch branch)

parseMultipleBranches :: Parser Branches
parseMultipleBranches = do
  branch <- parseBranch
  --trace ("Parsed branch in multiple branches: " ++ show branch) (return ())
  branches <- parseBranches
  return $ MultipleBranches branch branches
  --trace ("Parsed multiple branches: " ++ show branch ++ ", " ++ show branches) (return $ MultipleBranches branch branches)

-- <tree> ::= <name> <branches>
parseTree :: Parser Tree
parseTree = do
  name <- parseName
  --trace ("Parsed name: " ++ show name) (return ())
  branches <- parseBranches
  --trace ("Parsed branches: " ++ show branches) (return ())
  return $ Tree name branches

parsePlantTree :: Parser Query
parsePlantTree = do
  _ <- parseLiteral "plant"
  --trace ("Parsed plant") (return ())
  tree <- parseTree
  --trace ("Parsed tree: " ++ show tree) (return ())
  return $ PlantTree tree

parseCutBranch :: Parser Query
parseCutBranch = do
  _ <- parseLiteral "cut branch"
  --trace "Parsed cut branch" (return ())
  name <- parseName
  --trace ("Parsed name for cut branch: " ++ show name) (return ())
  return $ CutBranch name

parseCutBranches :: Parser Query
parseCutBranches = do
  _ <- parseLiteral "cut branches"
  --trace "Parsed cut branches" (return ())
  name <- parseName
  --trace ("Parsed name for cut branches: " ++ show name) (return ())
  return $ CutBranches name

parseCutTree :: Parser Query
parseCutTree = do
  _ <- parseLiteral "cut"
  --trace "Parsed cut" (return ())
  name <- parseName
  --trace ("Parsed name for cut tree: " ++ show name) (return ())
  return $ CutTree name

parseCutForest :: Parser Query
parseCutForest = do
  _ <- parseLiteral "cut forest"
  --trace "Parsed cut forest" (return ())
  return CutForest

parseInspectBranch :: Parser Query
parseInspectBranch = do
  _ <- parseLiteral "inspect branch"
  --trace "Parsed inspect branch" (return ())
  name <- parseName
  --trace ("Parsed name for inspect branch: " ++ show name) (return ())
  return $ InspectBranch name

parseInspectBranches :: Parser Query
parseInspectBranches = do
  _ <- parseLiteral "inspect branches"
  --trace "Parsed inspect branches" (return ())
  name <- parseName
  --trace ("Parsed name for inspect branches: " ++ show name) (return ())
  return $ InspectBranches name

parseInspectTree :: Parser Query
parseInspectTree = do
  _ <- parseLiteral "inspect"
  --trace "Parsed inspect" (return ())
  name <- parseName
  --trace ("Parsed name for inspect tree: " ++ show name) (return ())
  return $ InspectTree name

parseInspectForest :: Parser Query
parseInspectForest = do
  _ <- parseLiteral "inspect forest"
  --trace "Parsed inspect forest" (return ())
  return InspectForest

parsePlantCommand :: Parser Query
parsePlantCommand = parsePlantTree

parseCutCommand :: Parser Query
parseCutCommand =
  try parseCutBranch <|>
  try parseCutBranches <|>
  try parseCutTree <|>
  parseCutForest

parseInspectCommand :: Parser Query
parseInspectCommand =
  try parseInspectBranch <|>
  try parseInspectBranches <|>
  try parseInspectTree <|>
  parseInspectForest

parseCommands :: Parser Query
parseCommands =
  try parsePlantCommand <|>
  try parseCutCommand <|>
  parseInspectCommand

instance Arbitrary Query where
  arbitrary =
    PlantTree <$> arbitrary

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