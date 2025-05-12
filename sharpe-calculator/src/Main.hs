module Main where

import Loader
import Simulate

main :: IO ()
main = do
  wallets <- loadAllWallets "data/stock_combinations.txt" "data/prices.csv"

  mapM_ (\(i, wallet) -> do
            (sharpe, weights) <- computeBestSharpeAndWeights wallet
            putStrLn $ "Wallet " ++ show i ++ ": Best Sharpe = " ++ show sharpe
            saveBestSharpe "results/sharpeRatios.txt" i weights sharpe
        )
        (zip [1..] wallets)


