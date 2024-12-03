{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (isAlpha)
import Parsers qualified
import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf1, suchThat)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import qualified Lib3
import Lib2 (Query)
import Lib3 (renderQuery)

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
          Lib2.parseQuery (Lib3.renderQuery query) == Right query
    ]