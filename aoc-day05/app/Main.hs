module Main where

import Control.Arrow ((&&&))
import Control.Monad (forever)
import Data.Char (toLower, toUpper, isLetter)
import Data.Function (fix)
import Data.List (maximum, nub)


type Input = String

reactsWith :: Char -> Char -> Bool
reactsWith a b = a /= b && (a == toUpper b || b == toUpper a)

next :: Input -> Input
next x@[_] = x
next (x:y:xs) | x `reactsWith` y = next xs
              | otherwise = x : next (y:xs)

collapse :: Input -> Input
collapse s | s == s' = s
           | otherwise = collapse s'
  where s' = next s


part1 :: Input -> Int
part1 = length . collapse

part2 :: Input -> Int
part2 s = let reduced = collapse s
              units = nub . map toUpper $ reduced
          in minimum . map length $ do
  unit <- units
  pure . collapse . filter (not . (`elem` [unit, toLower unit])) $ reduced

main :: IO ()
main = interact $ show . (part1 &&& part2) . filter isLetter
