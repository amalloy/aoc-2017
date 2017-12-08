module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Strict as M
import Data.Bool (bool)
import Data.List (foldl')

type Reg = String
type Regs = M.Map Reg Int

type Op = Int -> Int -> Int
data Mut = Mut Reg Op Int

type Test = Int -> Int -> Bool
data Cond = Cond Reg Test Int

data Instr = Instr Mut Cond

parse :: [String] -> Instr
parse [r1, op, amt, "if", r2, test, cmp] = Instr (Mut r1 op' (read amt))
                                                 (Cond r2 test' (read cmp))
  where op' = case op of
          "inc" -> (+)
          "dec" -> (-)
        test' = case test of
          ">" -> (>)
          "<" -> (<)
          ">=" -> (>=)
          "<=" -> (<=)
          "==" -> (==)
          "!=" -> (/=)

apply :: Regs -> Instr -> Regs
apply m (Instr (Mut w f amt) (Cond r test target))
  = bool m m' $ get r `test` target
  where m' = M.insert w (get w `f` amt) m
        get k = M.findWithDefault 0 k m

part1 :: [Instr] -> Int
part1 program = let regs = foldl' apply M.empty program
                in maximum . M.elems $ regs

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . map (parse . words) . lines
