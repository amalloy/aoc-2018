module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray, getAssocs)
import Data.Array.ST (STArray)
import Data.Set (Set, fromList)

type Coord = (Int, Int)
newtype Id = Id Int deriving (Eq, Ord, Show)
newtype Distance = Distance Int deriving (Eq, Ord, Show)
newtype Progress = Progress (Maybe (Distance, [Id])) deriving (Show)
type Grid s = STArray s Coord Progress

data Cursor = Cursor Distance Coord Id deriving (Eq, Ord, Show)

type Input = [Coord]

solve :: (Coord, Coord) -> ST s String
solve bounds = do
  grid <- newArray bounds (Progress Nothing) :: ST s (Grid s)

  show <$> getAssocs grid

part1 :: Input -> String
part1 coords = let [minX, minY, maxX, maxY] = do
                     cmp <- [minimum, maximum]
                     lookup <- [fst, snd]
                     pure . cmp . map lookup $ coords
               in runST $ solve ((minX, minY), (maxX, maxY))



part2 :: Input -> Int
part2 = undefined

parse :: String -> [Coord]
parse = map parseLine . lines
  where parseLine s = case break (== ',') s of
          (x, (_:y)) -> (read x, read y)

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
