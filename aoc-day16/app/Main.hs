module Main where

import Control.Applicative (many, some)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import Data.Char (isDigit, isSpace)
import Data.Function (on)
import qualified Data.IntMap.Strict as M
import Data.List (foldl', intersect)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Regex.Applicative (sym, psym, string, match)

type Input = ([Sample], [Instruction])

type Argument = Int
data Arguments = Arguments {in1, in2, out :: Argument} deriving Show
type Opcode = Int
data Instruction = Instruction {opcode :: Opcode, arguments :: Arguments} deriving Show
data ArgType = Register | Immediate deriving Show
type RegIndex = Int
type RegVal = Int
type Registers = M.IntMap RegVal
type Binary a = Int -> Int -> a
type Operation = Arguments -> Registers -> Int
type OpName = String

data Sample = Sample {before, after :: Registers, instruction :: Instruction} deriving Show

ops :: [(OpName, Operation)]
ops = concat [do (name, f) <- [("add", (+)), ("mul", (*)),
                               ("ban", (.&.)), ("bor", (.|.))]
                 (arg, suffix) <- suffixes
                 pure (name ++ suffix, bin f Register arg)
             ,do (arg, suffix) <- suffixes
                 pure ("set" ++ suffix, bin const arg Immediate)
             ,do (name, f) <- [("gt", (>)), ("eq", (==))]
                 (a1, a2, suffix) <- [(Immediate, Register, "ir"),
                                      (Register, Immediate, "ri"),
                                      (Register, Register, "rr")]
                 pure (name ++ suffix, bin (cmp f) a1 a2)
             ]
   where suffixes = [(Register, "r"), (Immediate, "i")]
         cmp :: Binary Bool -> Binary Int
         cmp f x y = bool 0 1 $ x `f` y
         bin :: Binary Int -> ArgType -> ArgType -> Operation
         bin f x y (Arguments a1 a2 out) regs = (f `on` (read regs)) (x, a1) (y, a2)
         read :: Registers -> (ArgType, Argument) -> Int
         read regs (Register, x) = M.findWithDefault 0 x regs
         read _ (Immediate, x) = x

apply :: Operation -> Arguments -> Registers -> Registers
apply op args regs = M.insert (out args) (op args regs) regs

interpretations :: Sample -> [OpName]
interpretations (Sample before after (Instruction _ args)) =
  [name | (name, op) <- ops, apply op args before == after]

disambiguate :: M.IntMap [OpName] -> [M.IntMap Operation]
disambiguate table = case M.lookupGE 0 table of
  Nothing -> pure mempty
  Just (code, possibilities) -> do
    let remaining = M.delete code table
    name <- possibilities
    let others = fmap (filter (/= name)) remaining
    solution <- disambiguate others
    pure $ M.insert code (fromMaybe (error "invalid opName") (lookup name ops)) solution

solve :: [Sample] -> Maybe (M.IntMap Operation)
solve = listToMaybe . disambiguate . foldl' eliminate universe
  where universe :: M.IntMap [OpName]
        universe = M.fromList [(idx, map fst ops) |
                               (idx, _) <- zip [0..] ops]
        eliminate :: M.IntMap [OpName] -> Sample -> M.IntMap [OpName]
        eliminate table s@(Sample _ _ (Instruction op _)) =
          M.adjust keep op table
          where keep ops = intersect ops (interpretations s)

part1 :: Input -> Int
part1 = length . filter ((>= 3) . length) . map interpretations . fst

part2 :: Input -> Maybe Int
part2 (samples, program) = do
  opTable <- solve samples
  let interpret regs (Instruction op args) = apply (opTable M.! op) args regs
  pure ((M.! 0) . foldl' interpret (M.fromList $ zip [0..3] (repeat 0)) $ program)


parse :: String -> Maybe Input
parse = match r
  where r = (,) <$> (spaces `between` sample) <*> (spaces *> (spaces `between` instr) <* spaces)
        spaces = many (psym isSpace)
        sample = (\b i a -> Sample b a i) <$> regs "Before" <*> instr <*> regs "After"
        regs s = mkSample <$> (string s *> sym ':' *> spaces *> intList <* spaces)
        mkSample = M.fromList . zip [0..]
        intList = sym '[' *> (sym ',' *> spaces) `between` int <* sym ']'
        int = read <$> some (psym isDigit)
        between sep item = (:) <$> item <*> many (sep *> item)
        instr = Instruction <$> int <*> args <* spaces
        args = Arguments <$> sint <*> sint <*> sint
        sint = spaces *> int


main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse
