module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray, getAssocs)
import Data.Array.ST (STArray)

type Coord = (Int, Int)
data Direction = North | West | South | East deriving (Show, Enum)
data Turn = GoStraight | TurnLeft | TurnRight deriving (Show, Enum)
data Cart = Cart {heading :: Direction, plan :: Turn} deriving Show
data Terrain = Empty | Straight | Intersection | WNES | ENWS deriving Show
data Tile = Tile Terrain (Maybe Cart) deriving Show
type Input = [String]

parseTile :: Char -> Maybe Tile
parseTile c = case c of
  ' ' -> tile Empty
  '|' -> tile Straight
  '-' -> tile Straight
  '+' -> tile Intersection
  '\\' -> tile WNES
  '/' -> tile ENWS
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
translate d (x, y) = case d of
  North -> (x, y - 1)
  East -> (x + 1, y)
  South -> (x, y + 1)
  West -> (x - 1, y)

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined

parse :: String -> Input
parse = lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
