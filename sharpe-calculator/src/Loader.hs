module Loader (loadAllWallets, loadCombinations, saveBestSharpe) where

import Data.List (elemIndex, intercalate)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import Control.Monad (unless)
import Control.Exception (handle, SomeException)
import Text.Read (readMaybe)

-- Main loading functions

loadCombinations :: FilePath -> IO [[String]]
loadCombinations file = do
  contents <- readFile file
  case lines contents of
    [] -> return []
    ls -> return $ map (splitOnComma . strip) ls
  where
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

loadAllWallets :: FilePath -> FilePath -> IO [[Double]]
loadAllWallets combinationsPath stocksPath = do
  wallets <- loadCombinations combinationsPath 
  stocksContent <- readFile stocksPath 
  let loadFromContent = loadWalletFromContent stocksContent
  mapM loadFromContent wallets

loadWalletFromContent :: String -> [String] -> IO [Double]
loadWalletFromContent contents selectedStocks = do
  case lines contents of
    [] -> return []
    (headerLine:rows) -> do
      let header = splitOnComma headerLine
          indexes = mapMaybe (`elemIndex` header) selectedStocks
          values = concatMap (extractValues indexes) rows
      return values

-- Helper functions with complete pattern matching

extractValues :: [Int] -> String -> [Double]
extractValues indexes line =
  let fields = splitOnComma line
  in mapMaybe (readMaybeAt fields) indexes

readMaybeAt :: [String] -> Int -> Maybe Double
readMaybeAt xs i =
  case drop i xs of
    (x:_) -> readMaybe x
    _ -> Nothing

splitOnComma :: String -> [String]
splitOnComma = splitOn ','

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [[]]
  where
    f :: Char -> [String] -> [String]
    f c [] = [[c]]
    f c l@(x:xs)
      | c == delim = []:l
      | otherwise  = (c:x):xs

-- File saving with error handling

saveBestSharpe :: FilePath -> Int -> [Double] -> Double -> IO ()
saveBestSharpe path comboIndex weights sharpe = 
  handle handler $ do
    fileExists <- doesFileExist path
    unless fileExists $ writeHeader path
    
    let formattedWeights = "[" ++ intercalate ";" (map show weights) ++ "]"
        csvLine = intercalate "," [show comboIndex, show sharpe, formattedWeights]
    
    appendFile path (csvLine ++ "\n")
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ "Error saving Sharpe ratio: " ++ show e

writeHeader :: FilePath -> IO ()
writeHeader path = writeFile path "ComboIndex,SharpeRatio,Weights\n"