module Loader (loadCombinations) where

import System.IO

loadCombinations :: FilePath -> IO [[String]]
loadCombinations file = do
  contents <- readFile file
  return $ map (splitOnComma . strip) (lines contents)
  where
    splitOnComma = splitOn ','
    splitOn delim = foldr f [[]] 
      where
        f c l@(x:xs) | c == delim = []:l
                     | otherwise  = (c:x):xs
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
