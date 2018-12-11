module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Lazy as M
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

fringe :: Coord -> Int -> [Coord]
fringe (x, y) size = [(x + size - 1, y + dy) | dy <- [0..size -1]]
                  <> [(x + dx, y + size - 1) | dx <- [0..size - 2]]

bestSquare :: [Int] -> Int -> ((Int, Int), Int)
bestSquare widths serial = fst . maximumBy (comparing snd) . M.assocs $ subSquares
  where gridSize = 300
        subSquares = M.fromList $ do
          width <- widths
          x <- [1..gridSize - width + 1]
          y <- [1..gridSize - width + 1]
          let k = ((x, y), width)
          pure (k, squarePower k)

        squarePower (coord, 1) = powerLevel serial coord
        squarePower k@(coord, size) = let base = subSquares M.! (coord, size - 1)
                                      in base + sum (map (powerLevel serial)
                                                      (fringe coord size))


-- TODO: bestSquare [3] should work but doesn't, because it assumes recursive
-- subcalculations will be done.
part1 :: Input -> (Int, Int)
part1 = fst . bestSquare [1..3]

part2 :: Input -> ((Int, Int), Int)
part2 = bestSquare [1..298]

parse :: String -> Input
parse = read . head . lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
