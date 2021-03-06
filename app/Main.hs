module Main where

import Lib

import System.Environment
import System.Exit

import Flow

import Text.Layout.Table

import Data.List (elemIndices)
import Numeric

import qualified Data.Text as T

main = getArgs >>= parse >>= putStrLn . doWork
    where
      parse [] = putStrLn "Give a text file containing the measurements as an input!" >> exitWith (ExitFailure 1)
      parse (x:_) = readFile x

      possibleGridSizes = [2..10]

      tryGetGrids :: [Int] -> T.Text -> Maybe [Grid]
      tryGetGrids [] _ = Nothing
      tryGetGrids (x : xs) fileContents =
        case getGrids x fileContents of
          Just grids -> if not (null grids) then Just grids else Nothing
          Nothing    -> tryGetGrids xs fileContents

      doWork :: String -> String
      doWork fileContents =
        case tryGetGrids possibleGridSizes (T.pack fileContents) of
          Nothing -> "Could not parse grids!"
          Just grids ->
            let rowgroups = unifyGrids grids |> getCalculation |> map (map formatCalculation) |> map (colsAllG center)
                table = tableString (repeat def) unicodeS def rowgroups

            in "Found " ++ show (length grids) ++ " measurement grids...\n" ++ table


formatCalculation :: Calculation -> [String]
formatCalculation c =
  let tmp = [
              "Mean: "  ++ (fixPrecision longPrecision  (_mean c) |> addPlus)
            , "Min: "   ++ (fixPrecision shortPrecision (_min c)  |> addPlus)
            , "Max: "   ++ (fixPrecision shortPrecision (_max c)  |> addPlus)
            , "Range: " ++  fixPrecision shortPrecision (_range c)
            , "SD: "    ++  fixPrecision longPrecision  (_stddev c)
            ]
      longest = maximum $ map length tmp
      in map (padLine longest) tmp |> justify longest

    where
      shortPrecision = 3
      longPrecision = 6

      padLine :: Int -> String -> String
      padLine resultLength line
        | length line < resultLength = padLine resultLength insertSpace
        | otherwise    = line
          where firstSpace = elemIndices ' ' line |> head
                insertSpace = let (ys,zs) = splitAt firstSpace line in ys ++ " " ++ zs


      fixPrecision :: Int -> Double -> String
      fixPrecision numOfDecimals x = showFFloat (Just numOfDecimals) x ""

      addPlus :: String -> String
      addPlus x = if head x /= '-' then '+' : x else x
