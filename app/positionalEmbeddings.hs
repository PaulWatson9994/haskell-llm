module PositionalEmbeddings(positionalEmbeddings) where

-- generate all positional embeddings

type Embedding = [Double]

positionalEmbeddings :: Int -> Int -> Int -> [Embedding]
-- seqLength: maximum sequence length
-- embeddingDim: dimension of the embedding vector
-- n: user defined scalar (e.g. 100)
-- returns: the positional embedding for each position in the sequence
positionalEmbeddings seqLength embeddingDim n = [positionalEmbedding k embeddingDim n| k <- [0..(seqLength-1)]]

positionalEmbedding :: Int -> Int -> Int -> Embedding
-- converted from the Python in: https://machinelearningmastery.com/a-gentle-introduction-to-positional-encoding-in-transformer-models-part-1/
-- k: position in sequence
-- embeddingDim: dimension of the embedding vector
-- n: user defined scalar (e.g. 100)
-- returns the positional embedding for the kth position in the sequence 
positionalEmbedding k embeddingDim n= [ if isEven i  -- if even dimension
                                             then sin (fromIntegral k / (fromIntegral n ** (fromIntegral i / fromIntegral embeddingDim)))
                                             else cos (fromIntegral k / (fromIntegral n ** (fromIntegral (i-1) / fromIntegral embeddingDim)))
                                           | i <- [0..(embeddingDim-1)] ]

isEven :: Integral a => a -> Bool
-- test if integer is even
-- x: integer to test
-- returns True if x is even, False otherwise
isEven x = x `mod` 2 == 0
