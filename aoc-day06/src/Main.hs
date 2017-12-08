module Main where

import Control.Arrow ((&&&))
import Data.Coerce

import Data.Ord (Down(..))

iterations :: (a -> a) -> a -> [[a]]
iterations f x = go [x]
  where go all@(curr:prev) = all : go (f curr : all)

target :: [Int] -> (Int, Int)
target xs = let (sz, (Down idx)) = maximum $ zip xs (coerce [0 :: Int ..])
            in (idx, sz)

shuffle :: [Int] -> [Int]
shuffle xs = zipWith resize [0..] xs
  where (bank, size) = target xs
        len = length xs
        resize i amt = (size `div` len) + etc
          where etc | i == bank = 0
                    | size `mod` len < (i - bank) `mod` len = amt
                    | otherwise = amt + 1

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
