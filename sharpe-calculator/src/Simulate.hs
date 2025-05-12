module Simulate (simulateWallet, simulateAllWallets, generateWeights, computeBestSharpeAndWeights) where

import System.Random
import Data.List (intercalate)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parMap, rdeepseq, using)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V



-- best results for one wallet
optimizeWallet :: [Double] -> Int -> IO (Double, [Double])
optimizeWallet values nSets = do
    weightSets <- generateWeightSets (length values) nSets
    let sharpeResults = parMap rdeepseq (\ws -> (simulateWallet values ws, ws)) weightSets
        (bestSharpe, bestWeights) = maximumBy (comparing fst) sharpeResults
    return (bestSharpe, bestWeights)

computeBestSharpeAndWeights :: [Double] -> IO (Double, [Double])
computeBestSharpeAndWeights wallet = do
  weightsList <- replicateM 1000 (generateWeights 25)
  let results = parMap rdeepseq (\w -> (simulateWallet wallet w, w)) weightsList
  return $ maximumBy (comparing fst) results

simulateAllWallets :: [[Double]] -> Int -> IO [(Double, [Double])]
simulateAllWallets wallets nSets = do
    results <- mapM (\wallet -> optimizeWallet wallet nSets) wallets
    return results

generateWeights :: Int -> IO [Double]
generateWeights n = do
  ws <- replicateM n (randomRIO (0.2, 1.0))
  let total = sum ws
  return $ map (/ total) ws 

generateWeightSets :: Int -> Int -> IO [[Double]]
generateWeightSets walletSize nSets = replicateM nSets (generateWeights walletSize)

calculatePctChange :: [Double] -> [Double] -- n/n-1 - 1
calculatePctChange prices = zipWith (\x y -> (x - y) / y) (tail prices) prices

calculateReturns :: [Double] -> [Double] -> Double -- np.mean(returns) * 252 (annualized)
calculateReturns prices weights = mean (zipWith (*) prices weights) * 252
  where
    mean xs = sum xs / fromIntegral (length xs)
    
calculateVolatility :: [Double] -> Double -- np.sqrt(weights.T @ cov_matrix @ weights) * np.sqrt(252)
calculateVolatility returns =
  stdDev returns * sqrt 252
  where
    mean xs = sum xs / fromIntegral (length xs)
    stdDev xs = sqrt $ sum (map (\x -> (x - m) ^ 2) xs) / fromIntegral (length xs)
      where m = mean xs

calculateSharpe :: Double -> Double -> Double -- annual_return / volatility if volatility != 0 else 0
calculateSharpe _ 0 = 0
calculateSharpe annualReturn volatility = annualReturn / volatility

simulateWallet :: [Double] -> [Double] -> Double -- compose to calculateSharpe
simulateWallet values weights =
  let returns = calculatePctChange values
      ret = calculateReturns returns weights
      vol = calculateVolatility returns
  in calculateSharpe ret vol