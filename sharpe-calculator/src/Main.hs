module Main where

import Loader
import System.Directory (createDirectoryIfMissing)
import System.IO

main :: IO ()
main = do
  wallets <- loadCombinations "data/stock_combinations.txt"
  print wallets
