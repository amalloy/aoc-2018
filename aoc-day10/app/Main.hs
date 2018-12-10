module Main where

import Control.Arrow ((&&&))
import Control.Applicative (many)
import Data.List (maximum, minimum)
import System.Environment (getArgs)
import Text.Regex.Applicative (string, sym, psym, (=~))

type Coord a = (a, a)
type BoundingBox a = (Coord a, Coord a)
data Point = Point {position, velocity:: Coord Int} deriving Show
type Input = [Point]

boundingBox :: [Point] -> BoundingBox Int
boundingBox points = ((minimum . map fst $ ps,
                       minimum . map snd $ ps),
                      (maximum . map fst $ ps,
                       maximum . map snd $ ps))
  where ps = map position points

plausibleSolution :: BoundingBox Int -> Bool
plausibleSolution ((x, y), (x', y')) = abs (x' - x) < 800 && abs (y' - y) < 80

stepTimeBy :: Int -> Point -> Point
stepTimeBy dt (Point (x, y) v@(dx, dy)) = Point (x + dx * dt, y + dy * dt) v

display :: BoundingBox Int -> [Point] -> String
display ((minx, miny), (maxx, maxy)) ps = unlines $ do
  y <- [miny..maxy]
  pure $ do
    x <- [minx..maxx]
    pure $ glyph (x, y)
  where glyph p | any (== p) locs = '#'
                | otherwise = '.'
        locs = map position ps

prompt :: String -> IO Bool
prompt s = do
  putStrLn (s ++ "? (y/n)")
  (== "y") <$> getLine

part1 :: Input -> IO ()
part1 points = go 10000 $ map (stepTimeBy 10000) points
  where go t points = do let box = boundingBox points
                         print t
                         if plausibleSolution box
                           then do putStrLn . display box $ points
                                   done <- prompt "Look good"
                                   if done
                                     then pure ()
                                     else keepGoing
                           else keepGoing
           where keepGoing = go (t + 1) $ map (stepTimeBy 1) points



part2 :: Input -> IO ()
part2 points = pure ()

parse :: String -> Maybe Point
parse = (=~ r)
  where r = Point <$> point "position" <* sym ' ' <*> point "velocity"
        point name = string name *> string "=<" *> pair <* sym '>'
        pair = (,) <$> int <* sym ',' <*> int
        int = read <$> many (psym (/= ',' ))

main :: IO ()
main = do
  [file] <- getArgs
  points <- traverse parse . lines <$> readFile file
  mapM_ part1 points
  mapM_ part2 points
