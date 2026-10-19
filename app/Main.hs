module Main where

import Tokenise (tokenise, tokenToIdMapper, textToIds)
-- import TestTokenise (testTokenise)
import qualified Data.Map as Map
import InputTargetPairs (generateInputTargetPairs)

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


-- Read a text file from a local file path
readFileToString :: FilePath -> IO String
readFileToString = readFile


main :: IO ()
-- https://en.wikisource.org/wiki/The_Verdict
bookFileLocation :: String
bookFileLocation = "C:/git/haskell-llm/data/TheVerdict.txt"
-- Test with a local file:
main = do
  -- testTokenise
  content <- readFileToString bookFileLocation
  putStrLn "First 500 token Ids:"
  let tokenMapper :: Map.Map String Int
      tokenMapper = tokenToIdMapper (tokenise content) in
        print $ generateInputTargetPairs 5 (take 500 (textToIds content tokenMapper))   
