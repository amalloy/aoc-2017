module Main where

import Control.Arrow ((&&&))

import Data.Ord (Down(..))

iterations :: (a -> a) -> a -> [[a]]
iterations f x = go [x]
  where go all@(curr:prev) = all : go (f curr : all)

target :: [Int] -> (Int, Int)
target xs = let (sz, (Down idx)) = maximum $ zip xs (map Down [0..])
            in (idx, sz)

shuffle :: [Int] -> [Int]
shuffle xs = let (bank, size) = target xs
                 len = length xs
                 resize i amt = (size `div` len) + etc
                   where etc | i == bank = 0
                             | size `mod` len < (i - bank) `mod` len = amt
                             | otherwise = amt + 1
             in zipWith resize [0..] xs

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = not $ x `elem` xs

solve :: [Int] -> [[Int]]
solve = head . dropWhile unique . iterations shuffle

part1 :: [[Int]] -> Int
part1 = pred . length

part2 :: [[Int]] -> Int
part2 (x:xs) = 1 + length (takeWhile (/= x) xs)

main :: IO ()
main = interact $ show . (part1 &&& part2) . solve . map read . words . head . lines
