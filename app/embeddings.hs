module Embeddings (embeddingInit, extractMatRows) where
import Data.Matrix ( Matrix, fromList, fromLists, toLists) -- https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html
import System.Random (mkStdGen, randomRs)

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


-- Create initial embedding matrix for a given vocabulary size and output dimension 
--- with random values in a specified range
--- It's equivalent to PyTorch's torch.nn.Embedding
embeddingInit:: Int -> Int -> Int -> (Double, Double) -> Matrix Double
embeddingInit vocabSize outputDim seed range =      -- range is (minVal, maxVal)
    let gen = mkStdGen seed                     -- Create a random generator with the given seed
        randomFloats = take (vocabSize * outputDim) (randomRs range gen :: [Double])
    in fromList vocabSize outputDim randomFloats  -- create the matrix using the random values

-- extract rows from a Matrix given a list of indices
-- p43
extractMatRows:: Matrix Double -> [Int] -> Matrix Double
extractMatRows m indices = fromLists $ map (\i-> listMat !! i) indices
                           where listMat = toLists m

positionalEmbeddings :: Int -> Int -> Int -> (Double, Double) -> Matrix Double
positionalEmbeddings maxLength outputDim seed range = extractMatRows (embeddingInit maxLength outputDim seed range) [0..(maxLength - 1)]
