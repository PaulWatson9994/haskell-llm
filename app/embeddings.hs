module Embeddings (initialVocabularyEmbeddings) where
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

type Embedding = [Double]

-- Create initial embedding matrix for a given vocabulary size and output dimension 
--- with random values in a specified range
-- It's equivalent to PyTorch's torch.nn.Embedding
-- embeddingInit:: Int -> Int -> Int -> (Double, Double) -> Matrix Double
-- embeddingInit vocabSize outputDim seed range =       -- range is (minVal, maxVal)
--     let gen = mkStdGen seed                          -- Create a random generator with the given seed
--        randomFloats = take (vocabSize * outputDim) (randomRs range gen :: [Double])
--     in fromList vocabSize outputDim randomFloats     -- create the matrix using the random values


initialVocabularyEmbeddings:: Int -> Int -> Int -> (Double, Double) -> [Embedding]
-- Create a list of initial embeddings for a vocabulary
-- vocabSize: number of tokens in the vocabulary
-- embeddingDim: dimension of each embedding vector
-- seed: random seed for reproducibility
-- range: tuple specifying the range of random values (minVal, maxVal)
-- returns: a list holding an embedding for each token in the vocabulary
initialVocabularyEmbeddings vocabSize embeddingDim seed range = 
    let gen = mkStdGen seed
        randomFloats = take (vocabSize * embeddingDim) (randomRs range gen :: [Double])
    in [ take embeddingDim (drop (i * embeddingDim) randomFloats) | i <- [0..(vocabSize-1)] ]

