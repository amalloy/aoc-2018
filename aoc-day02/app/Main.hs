module Main where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (catMaybes)

type Input = [String]

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = go M.empty
  where go m [] = m
        go m (x:xs) = go (M.insertWith (+) x 1 m) xs

part1 :: Input -> Int
part1 words = let freqs = map (M.toList . frequencies) words
                  has n = length . filter (any ((== n) . snd)) $ freqs
              in has 2 * has 3


part2 :: Input -> Maybe String
part2 [] = Nothing
part2 (word:more) = case filter (match word) more of
                      [] -> part2 more
                      [hit] -> Just . catMaybes $ zipWith trim word hit
                      _ -> Nothing
  where match x y = (== 1) . length . filter not $ zipWith (==) x y
        trim x y | x == y = Just x
                 | otherwise = Nothing

parse :: String -> Input
parse = lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
