{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Lib2
import Lib3
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC


--import Debug.Trace (trace)
--trace ("Parsed name: " ++ show name ++ ", Remaining: " ++ show rest1) $

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "Parsing case 1 - give a better name" $
        Lib2.parseQuery "" @?= Left "Command cannot be empty",
      testCase "Parsing case 2 - give a better name" $
        Lib2.parseQuery "o" @?= Left "Invalid command"
    ]


propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ QC.testProperty "parseQuery . renderQuery == Right query" $
        \query ->
          --trace ("query: " ++ show query) $
          --trace ("Lib2.parseQuery (Lib3.renderQuery query): " ++ show (Lib2.parseQuery (Lib3.renderQuery query))) $
          Lib2.parseQuery (Lib3.renderQuery query) == Right query
    ]