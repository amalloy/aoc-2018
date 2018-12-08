{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, sepEndBy1)

data Node a = Node {metadata :: [a], children :: [Node a]} deriving (Show, Functor, Foldable)

int :: Parser Int
int = read <$> many1 digit

node :: Parser (Node Int)
node = do
  numChildren <- int
  spaces
  numMeta <- int
  children <- replicateM numChildren (spaces *> node)
  meta <- replicateM numMeta (spaces *> int)
  pure $ Node meta children

type Input = Node Int

part1 :: Input -> Int
part1 = sum

part2 :: Input -> Int
part2 = undefined


main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse node "stdin"
