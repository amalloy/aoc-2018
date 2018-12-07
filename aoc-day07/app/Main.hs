module Main where

import Text.Regex.Applicative (string, anySym, (=~))
import Data.List (minimum)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow ((&&&))
import Control.Monad (guard)
data Rule = Rule {prereq, goal :: Char} deriving Show
type RuleSet = M.Map Char (S.Set Char)

part1 :: RuleSet -> String
part1 = go S.empty
  where go done rules | M.null rules = ""
                      | otherwise = c : go (S.insert c done) (M.delete c rules)
          where c = minimum $ do
                  (goal, prereqs) <- M.assocs rules
                  guard $ prereqs `S.isSubsetOf` done
                  pure goal

part2 :: RuleSet -> Int
part2 = const 0

parse :: String -> Maybe Rule
parse = (=~ regex)
  where regex = Rule <$> (string "Step " *> anySym)
                     <*> (string " must be finished before step " *> anySym <* string " can begin.")

ruleGraph :: [Rule] -> RuleSet
ruleGraph rules = graph `M.union` goals
  where graph = M.fromListWith S.union [(goal rule, S.singleton (prereq rule)) | rule <- rules]
        goals = M.fromList [(prereq rule, S.empty) | rule <- rules]

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . fmap ruleGraph . traverse parse . lines
