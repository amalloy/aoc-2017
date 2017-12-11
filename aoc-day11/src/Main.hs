module Main where

import Control.Arrow ((&&&))

import Data.Monoid

type Coord a = (Sum a, Sum a, Sum a)

dir :: String -> Coord Int
dir s = (Sum x, Sum y, Sum z)
  where (x, y, z) = case s of
          "n" -> (0, 1, -1)
          "s" -> (0, -1, 1)
          "ne" -> (1, 0, -1)
          "sw" -> (-1, 0, 1)
          "nw" -> (-1, 1, 0)
          "se" -> (1, -1, 0)

dist :: (Num a, Ord a) => Coord a -> a
dist (Sum x, Sum y, Sum z) = maximum . map abs $ [x,y,z]

part1 = dist . mconcat . map dir

part2 :: a -> ()
part2 = const ()

split :: String -> [String]
split s = case break (== ',') s of
  (x, (',':more)) -> x : split more
  (x, "") -> [x]

main :: IO ()
main = interact $ show . (part1 &&& part2) . split . head . lines
