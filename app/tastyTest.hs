import Test.Tasty
    ( defaultMain, withResource, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure, Assertion, assertBool )

import Control.Exception (try, IOException)

import Tokenise (tokenise, tokenToIdMapper, textToIds)

import Embeddings (initialVocabularyEmbeddings)

import InputTargetPairs (generateInputTargetPairs)

import PositionalEmbeddings (positionalEmbeddings)

import ContextVector (contextVector, attentionWeights, allContextVectors)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Embedding = [Double]

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

contextTestEmbeddingsP57 :: [Embedding]
contextTestEmbeddingsP57 =  -- [RASCHKA26, p57]
    [[0.43, 0.15, 0.89], -- Your     (x^1)
     [0.55, 0.87, 0.66], -- journey  (x^2)
     [0.57, 0.85, 0.64], -- starts   (x^3)
     [0.22, 0.58, 0.33], -- with     (x^4)
     [0.77, 0.25, 0.10], -- one      (x^5)
     [0.05, 0.80, 0.55]] -- step     (x^6)

contextVectorP57 :: Embedding -- [RASCHKA26, p57]
contextVectorP57 = [0.4419, 0.6515, 0.5683]

contextVectorsP63 :: [Embedding] -- [RASCHKA26, p63]
contextVectorsP63 = 
       [[0.4421, 0.5931, 0.5790],
        [0.4419, 0.6515, 0.5683],
        [0.4431, 0.6496, 0.5671],
        [0.4304, 0.6298, 0.5510],
        [0.4671, 0.5910, 0.5266],
        [0.4177, 0.6503, 0.5645]]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, fileTests]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ 
  -- initialVocabularyEmbeddings vocabSize outputDim seed range
  -- p43
    testCase "Basic Embeddings Initialisation Test: Rows" $
        length (initialVocabularyEmbeddings 6 3 42 (-0.1, 0.1)) @?= 6

    -- initialVocabularyEmbeddings vocabSize outputDim seed range
    -- p43
  , testCase "Basic Embeddings Initialisation Test: Cols" $
        length (head (initialVocabularyEmbeddings 6 3 42 (-0.1, 0.1))) @?= 3

  , testCase "Basic Positional Embeddings Initialisation Test: Rows" $
        length (positionalEmbeddings 10 4 100) @?= 10

  , testCase "Basic Positional Embeddings Initialisation Test: Cols" $
        length (head (positionalEmbeddings 10 4 100)) @?= 4

  , testCase "Basic Attention Score Test" $
        length (attentionWeights contextTestEmbeddingsP57 2) @?= 6

  , testCase "Attention Scores Normalisation Test" $
        assertBool "Attention scores sum to 1" $
            abs (sum (attentionWeights contextTestEmbeddingsP57 2) - 1) <= 1e-6

  , testCase "Basic Context Vector Test" $
        assertApproxEqualEmbedding 1e-1 (contextVector contextTestEmbeddingsP57 2) contextVectorP57

  , testCase "All Context Vectors Test" $
        assertBool "All context vectors approximately equal" $
            all (uncurry (approxEqualLists 1e-1)) (zip (allContextVectors contextTestEmbeddingsP57) contextVectorsP63)
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

assertApproxEqualEmbedding:: Double -> Embedding -> Embedding -> Assertion
assertApproxEqualEmbedding tolerance expected actual =
    assertBool
        ("Expected " ++ show expected ++
         ", but got " ++ show actual ++
         " (tolerance " ++ show tolerance ++ ")")
        (all (\(expt, act) -> abs (expt - act) <= tolerance) (zip expected actual))

approxEqualLists :: Double -> [Double] -> [Double] -> Bool
approxEqualLists tolerance expected actual = all (\(expt, act) -> abs (expt - act) <= tolerance) (zip expected actual)
