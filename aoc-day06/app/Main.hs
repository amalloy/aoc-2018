{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (group, groupBy, sortBy, sort, minimum, maximum, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

type Coord = (Int, Int)
type Destination = (Id, Coord)
newtype Distance = Dist {getDist :: Int} deriving (Eq, Ord, Show)
newtype Id = Id Int deriving (Eq, Ord, Show, Enum)

distance :: Coord -> Coord -> Distance
distance (x, y) (x', y') = Dist $ abs (x - x') + abs (y - y')

bounds :: [Destination] -> (Coord, Coord)
bounds dests = ((minX, minY), (maxX, maxY))
  where points = map snd dests
        [minX, minY, maxX, maxY] = do
          cmp <- [minimum, maximum]
          lookup <- [fst, snd]
          pure . cmp . map lookup $ points

pointsWithin :: (Coord, Coord) -> [Coord]
pointsWithin ((minX,minY),(maxX,maxY)) = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]

type Input = [Destination]

part1 :: Input -> Int
part1 dests = let (owners, infinities) = foldMap consider points
                  candidates = filter (`S.notMember` infinities) owners
              in maximum . map length . group . sort $ candidates
  where box@((minX,minY),(maxX,maxY)) = bounds dests
        points = pointsWithin box
        infinite (x, y) = x `elem` [minX,maxX] || y `elem` [minY,maxY]
        consider point = let dists = map (fmap (distance point)) dests
                             closests = head . groupBy ((==) `on` snd) . sortBy (comparing snd) $ dists
                         in case closests of
                              [(id, dist)] -> ([id], S.fromList [id | infinite point])
                              _ -> mempty

part2 :: Input -> Int
part2 dests = length . filter (< 10000) $ do
  point <- points
  pure . sum . map (getDist . distance point . snd) $ dests
  where points = pointsWithin (bounds dests)

parse :: String -> [Coord]
parse = map parseLine . lines
  where parseLine s = case break (== ',') s of
          (x, (_:y)) -> (read x, read y)

main :: IO ()
main = interact $ show . (part1 &&& part2) . zip [Id 0..] . parse
