module InputTargetPairs (generateInputTargetPairs) where

import Data.List.Split (divvy)

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



-- Generate input-target pairs given a context length and a list of token IDs
-- This code generates pairs only where the full length of the context can be included
generateInputTargetPairs :: Int -> [Int] -> [([Int], Int)]
generateInputTargetPairs contextLength tokenIds = 
    map mkTargetPair $ divvy (contextLength+1) 1 tokenIds
    where mkTargetPair:: [Int] -> ([Int], Int)
          mkTargetPair ids = (init ids, last ids)
