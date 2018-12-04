module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Data.List (sortBy, foldl', maximumBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import Text.Parsec.Char (spaces, digit, char, string)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, parse, ParseError)

type Input = [Entry]

type GuardId = Int
data Instant = Instant {month, date, hour, minute :: Int} deriving (Show, Eq, Ord)
data Event = NewGuard GuardId | Sleep | Wake deriving Show
data Entry = Entry {timestamp :: Instant, event :: Event} deriving Show

int :: Parser Int
int = read <$> many1 digit

entry :: Parser Entry
entry = Entry <$> parseTime <* spaces <*> parseEvent

parseTime :: Parser Instant
parseTime = do
  char '['
  int
  char '-'
  month <- int
  char '-'
  day <- int
  spaces
  hour <- int
  char ':'
  minute <- int
  char ']'
  pure $ Instant month day hour minute

parseEvent :: Parser Event
parseEvent = newGuard <|> sleep <|> wake

newGuard :: Parser Event
newGuard = NewGuard <$> (string "Guard #" *> int <* string " begins shift")

sleep :: Parser Event
sleep = string "falls asleep" *> pure Sleep

wake :: Parser Event
wake = string "wakes up" *> pure Wake

sleepTimes :: [Entry] -> [(GuardId, Instant)]
sleepTimes = go 0
  where go _ [] = []
        go _ (Entry _ (NewGuard id):xs) = go id xs
        go g (Entry start Sleep : Entry end Wake:xs) =
          let (Instant m d h _) = start
              more = go g xs
          in [(g, Instant m d h t) | t <- [minute start..minute end - 1]] ++ more

frequencies :: Ord k => [k] -> M.Map k Int
frequencies = foldl' (\m x -> M.insertWith (+) x 1 m) M.empty

mostCommon :: Ord v => M.Map k v -> k
mostCommon = fst . maximumBy (comparing snd) . M.toList

part1 :: Input -> Int
part1 entries = let minutes = fmap (fmap minute) (sleepTimes entries)
                    sleepiness = frequencies (map fst minutes)
                    id = mostCommon sleepiness
                    napTime = frequencies . map snd . filter ((== id) . fst) $ minutes
                    min = mostCommon napTime
                in id * min

part2 :: Input -> Int
part2 entries = let minutes = fmap (fmap minute) (sleepTimes entries)
                    sleepiness = frequencies minutes
                    (id, min) = mostCommon sleepiness
                in id * min

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) .
  fmap (sortBy (comparing timestamp)) .
  traverse (parse entry "stdin") . lines
