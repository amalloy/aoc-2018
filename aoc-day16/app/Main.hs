module Main where

import Control.Applicative (many, some)
import Control.Arrow ((&&&))
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import Data.Char (isDigit, isSpace)
import Data.Function (on)
import qualified Data.IntMap.Strict as M
import Debug.Trace
import Text.Regex.Applicative (sym, psym, string, match, RE)

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


part1 :: Input -> Int
part1 = length . filter ((>= 3) . length) . map interpretations . fst

part2 :: Input -> Int
part2 = const 0

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
instr = Instruction <$> int <* spaces <*> args <* spaces
args = collect <$> spaces `between` int
collect [a,b,c] = Arguments a b c
collect xs = traceShow xs $ undefined

main :: IO ()
main = interact $ show . fmap (part1 &&& part2) . parse
