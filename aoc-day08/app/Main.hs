{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Function (on)
import Data.Functor.Foldable (Fix(..), cata)
import qualified Data.IntMap.Lazy as M
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, sepEndBy1)

data NodeF a b = NodeF {children :: [b], meta :: [a]} deriving (Show, Functor)
type Node a = Fix (NodeF a)

mkNode x y = Fix (NodeF x y)

int :: Parser Int
int = read <$> many1 digit

node :: Parser (Node Int)
node = do
  numChildren <- int
  spaces
  numMeta <- int
  mkNode <$> replicateM numChildren (spaces *> node) <*> replicateM numMeta (spaces *> int)

type Input = Node Int

part1 :: Input -> Int
part1 = cata (liftA2 ((+) `on` sum) children meta)

part2 :: Input -> Int
part2 = cata go
  where go (NodeF [] m) = sum m
        go (NodeF cs m) = let childValues = M.fromList . zip [1..] $ cs
                          in sum . map (flip (M.findWithDefault 0) childValues) $ m

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse node "stdin"
