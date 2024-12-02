{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Lib2 (State, emptyState, stateTransition, Query(..))
import Lib3 (renderStatements, parseStatements)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Command cannot be empty"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list),
    QC.testProperty "renderStatements . parseStatements == id" $
      \s -> let statements = renderStatements s
            in parseStatements statements == Right (s, "")
  ]