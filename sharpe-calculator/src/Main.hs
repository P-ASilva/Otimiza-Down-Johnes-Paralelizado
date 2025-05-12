module Main where

import Loader
import Simulate
import Types 
import System.Directory (createDirectoryIfMissing)
import System.IO

import System.Directory (doesFileExist)


main :: IO ()
main = do
  -- combinations <- loadCombinations "data/stock_combinations.txt"
  -- let total = length combinations
  -- wallet <- loadWallet  "data/prices.csv" (combinations !! 0)
  
  wallets <- loadAllWallets "data/stock_combinations.txt" "data/prices.csv"
  weights <- generateWeights 25
  let total = length wallets


  let wallet = wallets !! 0 

  let sharpeRatio = simulateWallet wallet weights
  print sharpeRatio



-- formatResult :: CalculatorResult -> String
-- formatResult (CalculatorResult w ws s) =
--   "Wallet: " ++ show w ++ "\n" ++
--   "Weights: " ++ show ws ++ "\n" ++
--   "Sharpe Ratio: " ++ show s ++ "\n"
