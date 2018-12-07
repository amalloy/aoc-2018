module Main where

import Text.Regex.Applicative (string, anySym, (=~))
import Data.Char (ord)
import Data.List (minimum, find, sort)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow ((&&&))
import Control.Monad (guard)

type Time = Int
data Rule = Rule {prereq, goal :: Char} deriving Show
type RuleSet = M.Map Char (S.Set Char)
data Task = Task {job :: Char, remaining :: Time}

runnable :: RuleSet -> S.Set Char -> Maybe Char
runnable rules done = listToMaybe . sort $ do
  (goal, prereqs) <- M.assocs rules
  guard $ prereqs `S.isSubsetOf` done
  pure goal


part1 :: RuleSet -> String
part1 = go S.empty
  where go done rules | M.null rules = ""
                      | otherwise = c : go (S.insert c done) (M.delete c rules)
          where (Just c) = runnable rules done

timeNeeded :: Char -> Time
timeNeeded c = 61 + ord c - ord 'A'

part2 :: RuleSet -> String
part2 = go S.empty 5 []
  where go done workers pending rules | M.null rules && null pending = ""
                                      | otherwise = case find ((== 0) . remaining) pending of
                                          Just (Task j _) -> go (S.insert j done)
                                                                (workers + 1)
                                                                (filter ((/= j) . job) pending)
                                                                rules
                                          Nothing | workers > 0 -> case runnable rules done of
                                                      Nothing -> advanceTime
                                                      (Just c) -> c : go done (workers - 1) (Task c (timeNeeded c) : pending) (M.delete c rules)
                                                  | otherwise -> advanceTime
          where advanceTime = go done workers (map tick pending) rules
                tick (Task j t) = Task j (t - 1)





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
