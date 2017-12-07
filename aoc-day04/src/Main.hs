module Main where

import Data.List (nub, sort)
import Control.Arrow ((&&&))

part1 :: [[String]] -> Int
part1 = length . filter permissible
  where permissible xs = xs == nub xs

part2 :: [[String]] -> Int
part2 = part1 . map (map sort)

main :: IO ()
main = interact $ show . (part1 &&& part2) . map words . lines
