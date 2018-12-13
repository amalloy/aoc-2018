{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.List (tails, transpose)
import qualified Data.Map as M

data Indexed a = Ix {ix :: Int, v :: a} deriving (Show, Functor)
type Pots = [Indexed Bool]

type Five a = (a,a,a,a,a)
type Rule a = (Five a, a)
type Transition = M.Map (Five Bool) Bool
data Input = Input Pots Transition deriving Show

on :: Char -> Bool
on = (== '#')

score :: Pots -> Int
score pots = sum [i | Ix i True <- pots]

draw :: Pots -> String
draw s = (show . ix . head $ r) ++ map (bool '.' '#' . v) r
  where r = dropWhile (not . v) s

part1 :: Input -> Int
part1 (Input init r) = score $ iterate (step r) init !! 100

part2 :: Input -> IO ()
part2 (Input init r) = (mapM_ (putStrLn . draw) . take 200 . iterate (step r)) init

window :: Int -> [a] -> [[a]]
window n = takeWhile ((== n) . length) . transpose . take n . tails

stub :: Int -> Int -> a -> [Indexed a]
stub from to x = [Ix i x | i <- [from..to]]

step :: Transition -> Pots -> Pots
step r xs = let ((Ix i _):more) = xs
                (Ix j _) = last xs
                prev = stub (i-4) (i-1) False
                    <> xs
                    <> stub (j+1) (j+4) False
                in map advance . window 5 $ prev
  where advance neighborhood@[_,_,focus,_,_] = next <$ focus
          where [a,b,c,d,e] = map v neighborhood
                next = r M.! (a,b,c,d,e)

parseRule :: String -> Rule Bool
parseRule s = let [input,_,[output]] = words s
                  [a,b,c,d,e] = map on input
              in ((a,b,c,d,e), on output)

parse :: String -> Input
parse s = let (header:_:rules) = lines s
              ["initial","state:",pots] = words header
              initState = zipWith Ix [0..] . map on $ pots
              transition = M.fromList . map parseRule $ rules
          in Input initState transition

main :: IO ()
main = do
  input <- parse <$> getContents
  print (part1 input)
  part2 input
