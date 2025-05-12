module Loader (loadAllWallets, loadWallet, loadCombinations, saveBestSharpe) where

import Data.List (elemIndex, intercalate)
import Data.Maybe (mapMaybe)


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

loadWallet :: FilePath -> [String] -> IO [Double]
loadWallet file selectedStocks = do
  contents <- readFile file
  let (headerLine:rows) = lines contents
      header = splitOnComma headerLine
      indexes = mapMaybe (`elemIndex` header) selectedStocks
      values = concatMap (extractValues indexes) rows
  return values

loadAllWallets :: FilePath -> FilePath -> IO [[Double]]
loadAllWallets combinationsPath stocksPath = do
  wallets <- loadCombinations combinationsPath 
  stocksContent <- readFile stocksPath 
  let loadFromContent = loadWalletFromContent stocksContent
  mapM loadFromContent wallets

loadWalletFromContent :: String -> [String] -> IO [Double]
loadWalletFromContent contents selectedStocks = do
  let (headerLine:rows) = lines contents
      header = splitOnComma headerLine
      indexes = mapMaybe (`elemIndex` header) selectedStocks
      values = concatMap (extractValues indexes) rows
  return values

-- Extracts values from a line, given the indexes of stocks you care about
extractValues :: [Int] -> String -> [Double]
extractValues indexes line =
  let fields = splitOnComma line
  in mapMaybe (readMaybeAt fields) indexes

-- Safe read from list by index and parse to Double
readMaybeAt :: [String] -> Int -> Maybe Double
readMaybeAt xs i =
  case drop i xs of
    (x:_) -> case reads x of
               [(n, "")] -> Just n
               _         -> Nothing
    _ -> Nothing

splitOnComma :: String -> [String]
splitOnComma = splitOn ','

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [[]]
  where
    f c l@(x:xs) | c == delim = []:l
                 | otherwise  = (c:x):xs

saveBestSharpe :: FilePath -> Int -> [Double] -> Double -> IO ()
saveBestSharpe path comboIndex weights sharpe = do
  let comboIndexStr = "Combo Index: " ++ show comboIndex
      weightsStr = "Weights: " ++ show weights
      sharpeStr = "Sharpe Ratio: " ++ show sharpe
      content = unlines [comboIndexStr, weightsStr, sharpeStr, ""]
  appendFile path content
