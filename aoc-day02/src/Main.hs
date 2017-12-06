module Main where

import Control.Monad (guard)

import Control.Arrow ((&&&))

part1 :: [[Int]] -> Int
part1 rows = sum [maximum xs - minimum xs | xs <- rows]

part2 :: [[Int]] -> Int
part2 rows = sum $ do
  row <- rows
  x <- row
  y <- row
  guard $ x /= y
  guard $ y /= 0
  guard $ x `mod` y == 0
  return $ x `div` y

main :: IO ()
main = interact $ show . (part1 &&& part2) . map ((map read) . words) . lines
