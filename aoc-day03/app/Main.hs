module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, ParseError)

type Input = [Claim']

newtype XCoord = X Int deriving (Show, Eq, Ord)
newtype YCoord = Y Int deriving (Show, Eq, Ord)
newtype Length a = L a deriving (Show, Eq, Ord)
newtype Id = Id Int deriving (Show, Eq, Ord)
data Claim' = Claim' Id XCoord YCoord (Length XCoord) (Length YCoord) deriving Show
data Claim = Claim { x, y, width, height :: Int}
type Sheet = M.Map (XCoord, YCoord) Int

int :: Parser Int
int = read <$> many1 digit

pair :: Char -> Parser (Int, Int)
pair c = (,) <$> (spaces *> int) <* char c <*> int

claim :: Parser Claim'
claim = do
  char '#'
  id <- int
  spaces
  char '@'
  (x, y) <- pair ','
  char ':'
  (w, h) <- pair 'x'
  pure $ Claim' (Id id) (X x) (Y y) (L (X w)) (L (Y h))

locations :: Claim' -> [(XCoord, YCoord)]
locations (Claim' _ (X x) (Y y) (L (X w)) (L (Y h))) = do
  cx <- [x..x + w-1]
  cy <- [y..y + h-1]
  pure (X cx, Y cy)

frequencies :: [Claim'] -> Sheet
frequencies claims = foldl' (\m coord -> M.insertWith (+) coord 1 m) M.empty $ claims >>= locations

part1 :: Input -> Int
part1 = length . filter (> 1) . M.elems . frequencies

part2 :: Input -> [Int]
part2 claims = let freqs = frequencies claims
               in do
  c@(Claim' (Id id) _ _ _ _) <- claims
  guard $ all ((== Just 1) . (freqs M.!?)) (locations c)
  pure id

readClaim :: String -> Either ParseError Claim'
readClaim = parse claim "stdin"

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . traverse readClaim . lines
