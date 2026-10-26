module PositionalEmbeddingsTest(unitTests) where

import Test.Tasty
    ( defaultMain, withResource, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure, Assertion, assertBool )

import PositionalEmbeddings (positionalEmbeddings)


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


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Basic Positional Embeddings Initialisation Test: Rows" $
        length (positionalEmbeddings 10 4 100) @?= 10

  , testCase "Basic Positional Embeddings Initialisation Test: Cols" $
        length (head (positionalEmbeddings 10 4 100)) @?= 4

  ]
