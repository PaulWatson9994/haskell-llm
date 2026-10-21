import Test.Tasty
    ( defaultMain, withResource, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )
import Test.Tasty.SmallCheck as SC ( testProperty )
import Test.Tasty.QuickCheck as QC ( testProperty )

import Data.List (sort)

import Control.Exception (try, IOException)

import Tokenise (tokenise, tokenToIdMapper, textToIds)

import Embeddings (initialVocabularyEmbeddings)

import InputTargetPairs (generateInputTargetPairs)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- Copyright 2026 Newcastle University.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- you may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- https://hackage.haskell.org/package/tasty
-- run with:
--- cabal run tastyTest

type Embedding = [Double]

-- Safely load a file as Text
loadFile :: FilePath -> IO (Either IOException T.Text)
loadFile path = try (TIO.readFile path)

bookFileLocation :: FilePath
bookFileLocation = "C:/git/haskell-llm/data/TheVerdict.txt"

-- example Matrix from p42
exp42 :: [Embedding]
exp42 = [[ 0.3374, -0.1778, -0.1690],
         [ 0.9178,  1.5810,  1.3010],
         [ 1.2753, -0.2010, -0.1606],
         [-0.4015,  0.9666, -1.1481],
         [-1.1589,  0.3255, -0.6315],
         [-2.8400, -0.7849, -1.4096]]

exp43 :: [Embedding]
exp43 = [[ 1.2753, -0.2010, -0.1606],
         [-0.4015,  0.9666, -1.1481],
         [-2.8400, -0.7849, -1.4096],
         [ 0.9178,  1.5810,  1.3010]]  

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, fileTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
 -- , SC.testProperty "Fermat's last theorem" $
 --     \x y z n ->
 --       (n :: Integer) > 2 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1, 2] @?= GT     -- N.B. in hunit, @?= is an assertion that the left-hand side equals the right side. If they are not equal, the test fails.

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1, 2, 2] @?= GT

  , testCase "simple list equality test" $
      [1, 2, 3] `compare` [1, 2, 3] @?= EQ

  -- initialVocabularyEmbeddings vocabSize outputDim seed range
  -- p43
  , testCase "Basic Embeddings Initialisation Test: Rows" $
        length (initialVocabularyEmbeddings 6 3 42 (-0.1, 0.1)) @?= 6

    -- initialVocabularyEmbeddings vocabSize outputDim seed range
    -- p43
  , testCase "Basic Embeddings Initialisation Test: Cols" $
        length (head (initialVocabularyEmbeddings 6 3 42 (-0.1, 0.1))) @?= 3
  ]

fileTests :: TestTree

fileTests = 
    withResource (loadFile bookFileLocation)           -- acquire
               (\_ -> pure ())                       -- release (none needed)
               mkFileTests

mkFileTests :: IO (Either IOException T.Text) -> TestTree
mkFileTests getFileContent =
    testGroup "File tests"
  [ testCase "File can be read" $ do
        result <- getFileContent
        case result of
          Left err   -> assertFailure ("Failed to read file: " ++ show err)
          Right text -> text @?= text,  -- simple check that file exists, 
   testCase "Check Tokenisation" $ do
         result <- getFileContent
         case result of
            Left err   -> assertFailure ("Failed to read file: " ++ show err)
            Right text -> 
                    length (inputPairs (T.unpack text)) `compare` 500 @?= LT
  ]

inputPairs :: String -> [([Int], Int)]
inputPairs content = let tokenMapper = tokenToIdMapper (tokenise content) in 
   generateInputTargetPairs 5 (take 500 (textToIds content tokenMapper))
