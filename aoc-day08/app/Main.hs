{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import qualified Data.IntMap.Lazy as M
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, sepEndBy1)

data Node a = Node {children :: [Node a], metadata :: [a]} deriving (Show, Functor, Foldable)

int :: Parser Int
int = read <$> many1 digit

node :: Parser (Node Int)
node = do
  numChildren <- int
  spaces
  numMeta <- int
  Node <$> replicateM numChildren (spaces *> node) <*> replicateM numMeta (spaces *> int)

type Input = Node Int

part1 :: Input -> Int
part1 = sum

part2 :: Input -> Int
part2 (Node [] m) = sum m
part2 (Node cs m) = let childValues = M.fromList . zip [1..] . map part2 $ cs
                    in sum . map (flip (M.findWithDefault 0) childValues) $ m

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse node "stdin"
