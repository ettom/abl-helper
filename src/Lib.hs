{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Bifunctor
import Data.List.Split
import Flow
import Text.Read

import qualified Data.Text as T

type Grid = [[Double]]
type GroupedGrid = [Double]


data Calculation = Calculation {
                       _mean   :: Double
                     , _min    :: Double
                     , _max    :: Double
                     , _range  :: Double
                     , _stddev :: Double
                     }


mkCalculation :: [Double] -> Calculation
mkCalculation xs = Calculation {
                                _mean = mean',
                                _min = min',
                                _max = max',
                                _range = range',
                                _stddev = stddev'
                               }
  where
    mean' = avg xs
    min' = minimum xs
    max' = maximum xs
    range' = abs $ min' - max'
    stddev' = map ((^2) . (-) mean') xs |> avg |> sqrt

    avg :: (Fractional a) => [a] -> a
    avg x = total / fromIntegral counter
      where
        (counter, total) = runningSum x
        runningSum :: (Num a) => [a] -> (Int, a)
        runningSum = foldl (\acc x -> bimap (+ 1) (+ x) acc) (0,0)


getCalculation :: [[GroupedGrid]] -> [[Calculation]]
getCalculation = map (map mkCalculation)

getGrids :: Int -> T.Text -> Maybe [Grid]
getGrids gridSize x = let cleaned = T.lines x |> map cleanUp |> filter (/= T.empty)
                          chunks = chunksOf gridSize cleaned
                          result = traverse toGrid chunks
                      in if validateChunks chunks then result else Nothing
  where
    validateChunks :: [[T.Text]] -> Bool
    validateChunks = all (\z -> length z == gridSize)

    cleanUp :: T.Text -> T.Text
    cleanUp x = T.break (\z -> z == '-' || z == '+') x |> snd |> T.strip |> T.replace "+" ""

    toGrid :: [T.Text] -> Maybe Grid
    toGrid = mapM toRow
      where
        toRow :: T.Text -> Maybe [Double]
        toRow x
          | length cells == gridSize = T.words x |> traverse (\z -> readMaybe (T.unpack z) :: Maybe Double)
          | otherwise                = Nothing
          where cells = T.words x


unifyGrids :: [Grid] -> [[GroupedGrid]]
unifyGrids [x] = map (map (: [])) x
unifyGrids (x : xs) = gridZip (unifyGrids xs) x
  where
    gridZip :: [[GroupedGrid]] -> Grid -> [[GroupedGrid]]
    gridZip = zipWith (zipWith (flip (:)))
