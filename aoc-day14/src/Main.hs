module Main where

import KnotHash (knot)
import Data.Bits
import Data.Char (digitToInt)

import Control.Arrow ((&&&))

bits :: Int -> [Bool]
bits c = do
  bit <- [3,2..0]
  pure $ c .&. (1 `shiftL` bit) /= 0

row :: String -> Int -> [Bool]
row key idx = bits . digitToInt =<< knot (key ++ "-" ++ show idx)

grid :: Int -> String -> [[Bool]]
grid rows key = map (row key) [0..rows - 1]

part1 :: [[Bool]] -> Int
part1 = length . filter id . concat

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . grid 128 . head . lines
