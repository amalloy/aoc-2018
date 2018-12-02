module Main where

import Control.Arrow ((&&&))

type Input = [String]

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined

parse :: String -> Input
parse = lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
