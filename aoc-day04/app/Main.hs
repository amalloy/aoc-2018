module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
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

part1 :: Input -> Input
part1 = id

part2 :: Input -> Int
part2 = undefined



main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . traverse (parse entry "stdin") . lines
