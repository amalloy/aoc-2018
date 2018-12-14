module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray, getAssocs)
import Data.Array.ST (STArray)

type Coord = (Int, Int) -- Y, X because it sorts best
data Direction = North | West | South | East deriving (Show, Enum)
data Turn = GoStraight | TurnLeft | TurnRight deriving (Show, Enum)
data Cart = Cart {heading :: Direction, plan :: Turn} deriving Show
data Curve = SEWN | NEWS deriving (Show, Enum)
data Terrain = Empty | Straight | Intersection | Curve Curve deriving Show
data Tile = Tile Terrain (Maybe Cart) deriving Show

type Input = (Coord, [Tile])

parseTile :: Char -> Maybe Tile
parseTile c = case c of
  ' ' -> tile Empty
  '|' -> tile Straight
  '-' -> tile Straight
  '+' -> tile Intersection
  '\\' -> tile (Curve SEWN)
  '/' -> tile (Curve NEWS)
  'v' -> cart South
  '^' -> cart North
  '>' -> cart East
  '<' -> cart West
  _ -> Nothing
  where tile = Just . flip Tile Nothing
        cart dir = Just . Tile Straight . Just $ Cart dir TurnLeft

nextPlan :: Turn -> Turn
nextPlan t = case t of
  GoStraight -> TurnLeft
  TurnLeft -> TurnRight
  TurnRight -> GoStraight

turn :: Turn -> Direction -> Direction
turn GoStraight x = x
turn TurnLeft x = case x of
  North -> West
  West -> South
  South -> East
  East -> North
turn TurnRight x = case x of
  North -> East
  East -> South
  South -> West
  West -> North

translate :: Direction -> Coord -> Coord
translate d (y, x) = case d of
  North -> (y - 1, x)
  East -> (y, x + 1)
  South -> (y + 1, x)
  West -> (y, x - 1)

bounce :: Curve -> Direction -> Direction
bounce SEWN d = case d of
  South -> East
  East -> South
  West -> North
  North -> West
bounce NEWS d = case d of
  North -> East
  East -> North
  West -> South
  South -> West

part1 = id

part2 :: Input -> Int
part2 = const 0

parse :: String -> Maybe Input
parse = go (0, 0) . lines
  where go :: Coord -> [String] -> Maybe Input
        go pos [] = Just (pos, [])
        go (y, _) ([]:xs) = go (y + 1, 0) xs
        go (x, y) ((c:cs):more) = do
          tile <- parseTile c
          fmap (tile :) <$> go (x + 1, y) (cs:more)



main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse
