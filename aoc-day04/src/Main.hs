module Main where

import Data.List (nub)

part1 :: [[String]] -> Int
part1 = length . filter permissible
  where permissible xs = xs == nub xs

main :: IO ()
main = interact $ show . part1 . map words . lines
