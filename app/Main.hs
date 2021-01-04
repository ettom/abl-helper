{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Lib

import Control.Monad      (join)
import Data.Text.IO       (putStrLn, readFile)
import Flow
import Numeric
import System.Environment
import System.Exit
import Text.Layout.Table

import qualified Data.Text as T
import           Prelude   hiding (putStrLn, readFile)

shortPrecision = 3
longPrecision = 6
possibleGridSizes = [2..10]

main :: IO ()
main = getArgs >>= parse >>= putStrLn . makeOutput
    where
      parse [] = putStrLn "Give a text file containing the measurements as an input!" >> exitWith (ExitFailure 1)
      parse (x:_) = readFile x

      tryGetGrids :: [Int] -> T.Text -> Maybe [Grid]
      tryGetGrids [] _ = Nothing
      tryGetGrids (x : xs) fileContents =
        case getGrids x fileContents of
          Just grids -> if not (null grids) then Just grids else Nothing
          Nothing    -> tryGetGrids xs fileContents

      highlight :: T.Text -> T.Text -> T.Text -> T.Text
      highlight color target = T.replace (" " <> target <> " ") (color <> " " <> target <> " " <> revert)
        where
          revert = "\ESC[0m"

      makeOutput :: T.Text -> T.Text
      makeOutput fileContents =
        case tryGetGrids possibleGridSizes fileContents of
          Nothing -> "Could not parse grids!"
          Just grids ->
            let calculations = unifyGrids grids |> map (map mkCalculation)
                rowgroups = map (colsAllG center . map (map T.unpack . formatCalculation)) calculations
                table = tableString (repeat def) unicodeS def rowgroups
                stddevs = map _stddev $ join calculations
                ranges = map _range $ join calculations
                avg_stddev = fixPrecision longPrecision $ avg stddevs
                max_stddev = fixPrecision longPrecision $ maximum stddevs
                min_stddev = fixPrecision longPrecision $ minimum stddevs
                avg_range = fixPrecision shortPrecision $ avg ranges
                max_range = fixPrecision shortPrecision $ maximum ranges
                min_range = fixPrecision shortPrecision $ minimum ranges
                red = "\ESC[31m"
                green = "\ESC[32m"
                bright_red = "\ESC[91m"
                bright_green = "\ESC[92m"
                alignment = T.replicate (longPrecision - shortPrecision) " "

            in highlight bright_red max_stddev $
               highlight bright_green min_stddev $
               highlight green min_range $
               highlight red max_range $
                  "Found " <> T.pack (show $ length grids) <> " measurement grids...\n"
               <> "SD    - Mean: " <> avg_stddev <> " Max: " <> max_stddev <> " Min: " <> min_stddev <> " \n"
               <> "Range - Mean: " <> avg_range <> alignment <> " Max: " <> max_range <> alignment <> " Min: " <> min_range <> " \n"
               <> T.pack table

formatCalculation :: Calculation -> [T.Text]
formatCalculation c =
  let tmp = [
              "Mean: "  <> (fixPrecision longPrecision  (_mean c) |> addPlus)
            , "Min: "   <> (fixPrecision shortPrecision (_min c)  |> addPlus)
            , "Max: "   <> (fixPrecision shortPrecision (_max c)  |> addPlus)
            , "Range: " <>  fixPrecision shortPrecision (_range c)
            , "SD: "    <>  fixPrecision longPrecision  (_stddev c)
            ]
      longest = maximum $ map T.length tmp
      in map (padLine longest) tmp |> map T.unpack |> justify longest |> map T.pack

    where
      padLine :: Int -> T.Text -> T.Text
      padLine resultLength line
        | T.length line < resultLength = padLine resultLength insertSpace
        | otherwise                    = line
          where
            insertSpace = let (ys,zs) = T.breakOn " " line in ys <> " " <> zs

      addPlus :: T.Text -> T.Text
      addPlus x = if T.head x /= '-' then T.cons '+' x else x

fixPrecision :: Int -> Double -> T.Text
fixPrecision numOfDecimals x = T.pack $ showFFloat (Just numOfDecimals) x ""
