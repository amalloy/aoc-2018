module Main where

import Control.Arrow ((&&&))
import qualified Data.Map as M

type Input = [String]

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = go M.empty
  where go m [] = m
        go m (x:xs) = go (M.insertWith (+) x 1 m) xs

part1 :: Input -> Int
part1 words = let freqs = map (M.toList . frequencies) words
                  has n = length . filter (any ((== n) . snd)) $ freqs
              in has 2 * has 3


part2 :: Input -> Int
part2 x = 0

parse :: String -> Input
parse = lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
