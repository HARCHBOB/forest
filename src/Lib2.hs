{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fmax-pmcheck-models=100 #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    ) where

import Parsers


parseQuery :: String -> Either String Query
parseQuery "" = Left "Command cannot be empty"
parseQuery input =
  case parse parseCommands input of
    Right (query, _) -> Right query
    Left err         -> Left err

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
replaceTree newTree = map (\t -> case getTreeName t == getTreeName newTree of
    True  -> newTree
    False -> t)

findTree :: Name -> Forest -> Maybe Tree
findTree _ [] = Nothing
findTree name (t@(Tree n _):ts)
  | n == name = Just t
  | otherwise = findTree name ts
