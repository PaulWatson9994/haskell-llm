module Tokenise (tokenise, tokenToIdMapper, idToTokenMapper, textToIds) where

import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.List (nub, sort)

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

-- Create tokens from a string. Separate out punctuation as well as words
tokenise :: String -> [String]
tokenise = filter (not . null) . go
  where
    go [] = [endOfText]
    go (c:cs)
      | isSpace c = go cs                                         -- ignore whitespace
      | c == '-' && not (null cs) && head cs == '-' = "--" : go (tail cs)  -- "--" as single token
      | isPunctuation c = [c] : go cs                             -- create a token for punctuation
      | otherwise = let (word, rest) = span isWordChar (c:cs)     -- create a token for a word
                    in word : go rest

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ".,;:!?'\"-()[]{}/@#$%^&*+=~`|\\<>"

isWordChar :: Char -> Bool
isWordChar c = not (isSpace c) && not (isPunctuation c)

unknown :: String
unknown = "<unk>"

endOfText :: String
endOfText = "<endoftext>"

specialTokens :: [String]
specialTokens = [unknown, endOfText]

-- generate ids from list of tokens
tokenToIdMapper :: [String] -> Map.Map String Int
tokenToIdMapper tokens = Map.fromList (zip (sort uniqueTokens ++ specialTokens) [0..]) -- not sure why sorting is needed, but it is in the python version
  where
    uniqueTokens = nub tokens

idToTokenMapper :: Map.Map String Int -> Map.Map Int String
idToTokenMapper idMap = Map.fromList [(v, k) | (k, v) <- Map.toList idMap]

mapTokenToId:: String -> Map.Map String Int -> Int
mapTokenToId token tokenMap = Map.findWithDefault (tokenMap Map.! unknown) token tokenMap

textToIds:: String -> Map.Map String Int -> [Int]
textToIds text tokenMap = map (`mapTokenToId` tokenMap) (tokenise text)
