module Main where

import Loader
import Simulate
import Types 
import System.Directory (createDirectoryIfMissing)
import System.IO
import Control.Monad (replicateM)
import System.Directory (doesFileExist)


main :: IO ()
main = do
  -- combinations <- loadCombinations "data/stock_combinations.txt"
  -- let total = length combinations
  -- wallet <- loadWallet  "data/prices.csv" (combinations !! 0)
  
  wallets <- loadAllWallets "data/stock_combinations.txt" "data/prices.csv"
  let total = length wallets
  
  weightsList <- replicateM total (generateWeights 25)
  
  let sharpeResults = zipWith simulateWallet wallets weightsList
  
  -- mapM_ print walletSharpePairs
  

  let numberedResults = zip [1..] walletSharpePairs
  mapM_ (\(i, sharpe) -> putStrLn $ "Wallet " ++ show i ++ ": " ++ show sharpe) numberedResults



-- formatResult :: CalculatorResult -> String
-- formatResult (CalculatorResult w ws s) =
--   "Wallet: " ++ show w ++ "\n" ++
--   "Weights: " ++ show ws ++ "\n" ++
--   "Sharpe Ratio: " ++ show s ++ "\n"