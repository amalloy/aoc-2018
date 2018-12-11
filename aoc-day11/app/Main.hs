module Main where

import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.List (maximumBy)

type Coord = (Int, Int)
type Input = Int

powerLevel :: Input -> Coord -> Int
powerLevel serial (x, y) = let rackId = x + 10
                               base = rackId * y
                               large = (base + serial) * rackId
                               power = ((large `mod` 1000) `div` 100) - 5
                           in power

squarePower :: Int -> Input -> Coord -> Int
squarePower width serial (x, y) = sum . map (powerLevel serial) $ do
  dx <- [0..width - 1]
  dy <- [0..width - 1]
  pure (x + dx, y + dy)

part1 :: Input -> (Int, Int)
part1 serial = maximumBy (comparing (squarePower width serial)) $ do
  x <- [0..maxInit]
  y <- [0..maxInit]
  pure (x, y)
  where width = 3
        gridSize = 300
        maxInit = gridSize - width

part2 :: Input -> Int
part2 = const 0

parse :: String -> Input
parse = read . head . lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
