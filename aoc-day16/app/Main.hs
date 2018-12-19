module Main where

import Control.Arrow ((&&&))
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import Data.Function (on)
import qualified Data.IntMap.Strict as M

type Input = [String]

type Argument = Int
data Arguments = Arguments {in1, in2, out :: Argument}
data ArgType = Register | Immediate
type RegIndex = Int
type RegVal = Int
type Registers = M.IntMap RegVal
type Binary a = Int -> Int -> a
type Operation = Arguments -> Registers -> Int

ops :: [(String, Operation)]
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

part1 :: Input -> Int
part1 = const 0

part2 :: Input -> Int
part2 = const 0

parse :: String -> Input
parse = lines

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
