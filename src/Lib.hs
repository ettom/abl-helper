{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Bifunctor
import Data.List.Split
import Flow
import Text.Read

import qualified Data.Text as T

type Grid = [[Double]]
type GridsAggregated = [[[Double]]] -- A matrix of grid values (grouped by coordinates)
data Calculation = Calculation {
                       _mean   :: Double
                     , _min    :: Double
                     , _max    :: Double
                     , _range  :: Double
                     , _stddev :: Double
                     }

mkCalculation :: [Double] -> Calculation
mkCalculation xs = Calculation {
                                 _mean = avg xs
                               , _min = minimum xs
                               , _max = maximum xs
                               , _range = abs $ minimum xs - maximum xs
                               , _stddev = map ((^(2 :: Integer)) . (-) (avg xs)) xs |> avg |> sqrt
                               }

getGrids :: Int -> T.Text -> Maybe [Grid]
getGrids gridSize x = let cleaned = T.lines x |> map cleanUp |> filter (/= T.empty)
                          chunks = chunksOf gridSize cleaned
                          result = traverse toGrid chunks
                      in if validateChunks chunks then result else Nothing
  where
    validateChunks :: [[T.Text]] -> Bool
    validateChunks = all (\z -> length z == gridSize)

    cleanUp :: T.Text -> T.Text
    cleanUp text = T.break (\z -> z == '-' || z == '+') text |> snd |> T.strip |> T.replace "+" ""

    toGrid :: [T.Text] -> Maybe Grid
    toGrid = mapM toRow
      where
        toRow :: T.Text -> Maybe [Double]
        toRow text
          | length cells == gridSize = T.words text |> traverse (\z -> readMaybe (T.unpack z) :: Maybe Double)
          | otherwise                = Nothing
          where cells = T.words text


unifyGrids :: [Grid] -> GridsAggregated
unifyGrids [] = error "Logic error"
unifyGrids [x] = map (map (: [])) x
unifyGrids (x : xs) = gridZip (unifyGrids xs) x
  where
    gridZip :: GridsAggregated -> Grid -> GridsAggregated
    gridZip = zipWith (zipWith (flip (:)))

avg :: (Fractional a) => [a] -> a
avg x = total / fromIntegral counter
  where
    (counter, total) = runningSum x
    runningSum :: (Num a) => [a] -> (Int, a)
    runningSum = foldl (\acc y -> bimap (+ 1) (+ y) acc) (0,0)
