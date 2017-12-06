module Main where

solve :: [[Int]] -> Int
solve rows = sum [maximum xs - minimum xs | xs <- rows]

main :: IO ()
main = interact $ show . solve . map ((map read) . words) . lines
