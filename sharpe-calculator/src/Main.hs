module Main where

import Loader
import Simulate
import Types 
import System.Directory (createDirectoryIfMissing)
import System.IO


main :: IO ()
main = do
  wallets <- loadCombinations "data/stock_combinations.txt"

  let total = length wallets
  weights <- generateWeights 25
  -- test fo 1 wallet
  let wallet = wallets !! 0
  walletValues <- loadWallet "data/dow_jones_2024_S2_closing_prices.csv" wallet
  let sharpeRatio = simulateWallet walletValues weights
  print sharpeRatio



-- formatResult :: CalculatorResult -> String
-- formatResult (CalculatorResult w ws s) =
--   "Wallet: " ++ show w ++ "\n" ++
--   "Weights: " ++ show ws ++ "\n" ++
--   "Sharpe Ratio: " ++ show s ++ "\n"
