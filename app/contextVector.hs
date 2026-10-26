module ContextVector(contextVector, attentionWeights, allAttentionWeights, allContextVectors) where
import Data.List (foldl', foldl1)

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

contextVector:: [Embedding] -> Int -> Embedding
-- Compute the context vector for the embedding at the given index against all embeddings in a sequence
-- embeddings: list of all embeddings in a sequence [Rachka25, p60]
-- index: the index of the embedding to compute the context vector for
-- result: the context vector for the embedding at the given index
contextVector embeddings index = 
    sumEmbeddings weightedEmbeddings
    where scores = attentionWeights embeddings index
          weightedEmbeddings = zipWith embeddingTimesWeight embeddings scores

allAttentionWeights :: [Embedding] -> [[Double]]
-- Compute the attention weights for all tokens in the sequence
-- embeddings: list of all embeddings in a sequence
-- result: list of attention weights for all tokens against all embeddings
allAttentionWeights embeddings = map (attentionWeights embeddings) [0..length embeddings - 1]

allContextVectors:: [Embedding] -> [Embedding]
allContextVectors embeddings = map (contextVector embeddings) [0..length embeddings - 1]

attentionScores :: [Embedding] -> Int -> [Double]
-- Compute the attention scores for the embedding at the given index against all embeddings in a sequence
-- embeddings: list of all embeddings in a sequence
-- index: the index of the embedding to compute attention weights for
-- result: list of attention scores for the embedding at the given index against all embeddings
attentionScores embeddings index = map (\e -> dotProduct (embeddings !! index) e) embeddings

attentionWeights :: [Embedding] -> Int -> [Double]
-- Compute the attention weights (normalized attention scores) for the embedding at the given index against all embeddings in a sequence
-- embeddings: list of numbers to normalise
-- index: the index of the embedding to compute attention weights for
-- result: attention weights for the embedding at the given index against all embeddings
attentionWeights embeddings index = softMax $ attentionScores embeddings index

dotProduct :: Embedding -> Embedding -> Double
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

softMax :: [Double] -> [Double]
-- Use softMax to normalise a list of value
softMax xs = let maxX = maximum xs
                 exps = map (\x -> exp (x - maxX)) xs
                 sumExps = foldl' (+) 0 exps
                 in map (/ sumExps) exps

embeddingTimesWeight:: Embedding -> Double -> Embedding
-- Multiply an embedding by a scalar weight
-- embedding: the embedding to be scaled
-- weight: the scalar weight to multiply the embedding by
-- result: the scaled embedding
embeddingTimesWeight embedding weight = map (* weight) embedding

sumEmbeddings :: [Embedding] -> Embedding
-- Sum a list of embeddings element-wise
-- embeddings: list of embeddings to sum
-- result: the element-wise sum of the embeddings
sumEmbeddings = foldl1 sum2Embeddings
    where sum2Embeddings e1 e2 = zipWith (+) e1 e2
