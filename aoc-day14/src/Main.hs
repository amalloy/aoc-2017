module Main where

import KnotHash (knot)

import Control.Arrow ((&&&))

part1 :: a -> ()
part1 = const ()

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . lines
