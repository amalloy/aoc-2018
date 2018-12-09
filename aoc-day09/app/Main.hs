module Main where

import Control.Arrow ((&&&))
import qualified Data.IntMap as M
import qualified Data.Sequence as S
import Data.Foldable (toList)

data Input = Input {numPlayers, lastMarble :: Int}

data Direction = Clockwise | Counterclockwise
newtype Magnitude = Magnitude Int
newtype Marble = Marble {unMarble :: Int}

data Game = Game {scores :: M.IntMap Int,
                  currentIndex :: Int,
                  rules :: Input,
                  bag :: [Marble],
                  circle :: S.Seq Marble
                 }

display :: Game -> String
display g = unwords . zipWith draw [0..] . map (show . unMarble) . toList . circle $ g
  where draw i s | i == curr = "(" ++ s ++ ")"
                 | otherwise = s
        curr = currentIndex g


dir :: Direction -> Int
dir Clockwise = 1
dir Counterclockwise = -1

move :: Magnitude -> Direction -> Int -> Int
move (Magnitude m) d x = x + (m * dir d)

turn :: Game -> Game
turn (Game scs offs rules (Marble m:ms) circle) | m `mod` 23 == 0 = score
                                                | otherwise = place
  where size = S.length circle
        score = Game (M.adjust (+ (current + m)) (m `mod` (numPlayers rules)) scs)
                     ((toRemove + newSize) `mod` newSize)
                     rules
                     ms
                     (S.deleteAt toRemove circle)
          where newSize = size - 1
                toRemove = (move (Magnitude 7) Counterclockwise offs + size) `mod` size
                current = unMarble $ S.index circle toRemove
        place = Game scs newOffset rules ms
                     (S.insertAt newOffset (Marble m) circle)
          where newOffset = (move (Magnitude 2) Clockwise offs + (5 * size)) `mod` size

initialGame :: Input -> Game
initialGame rules = Game (M.fromList . zip [0..numPlayers rules] $ (repeat 0))
                         0
                         rules
                         (map Marble [1..lastMarble rules])
                         (S.singleton (Marble 0))

part1 :: Input -> Int
part1 = go . initialGame
  where go g | null . bag $ g = maximum . M.elems . scores $ g
             | otherwise = go (turn g)

part2 :: Input -> Int
part2 = undefined

parse :: String -> Input
parse s = let ws = map read . words . head . lines $ s
          in Input (ws !! 0) (ws !! 6)

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
