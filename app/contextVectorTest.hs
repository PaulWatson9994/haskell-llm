module ContextVectorTest(unitTests) where

import Test.Tasty
    ( defaultMain, withResource, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure, Assertion, assertBool )

import Control.Exception (try, IOException)

import ContextVector (contextVector)

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, fileTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Basic Context Vector Test" $
        assertApproxEqualEmbedding 1e-1 (contextVector contextTestEmbeddingsP57 2) contextVectorP57
  ]

assertApproxEqualEmbedding:: Double -> Embedding -> Embedding -> Assertion
assertApproxEqualEmbedding tolerance expected actual =
    assertBool
        ("Expected " ++ show expected ++
         ", but got " ++ show actual ++
         " (tolerance " ++ show tolerance ++ ")")
        (all (\(expt, act) -> abs (expt - act) <= tolerance) (zip expected actual))
