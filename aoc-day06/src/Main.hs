module Main where

import Control.Arrow ((&&&))

import Data.List (maximumBy)
import Data.Ord (comparing, Down(..))

data EvacOrder = Evac Int Int

iterations :: (a -> a) -> a -> [[a]]
iterations f x = go [x]
  where go all@(curr:prev) = all : go (f curr : all)

target :: [Int] -> EvacOrder
target xs = let (sz, (Down idx)) = (maximum $ zip xs (map Down [0..]))
            in Evac idx sz

shuffle :: [Int] -> [Int]
shuffle xs = let t@(Evac bank size) = target xs
                 len = length xs
                 resize i amt | i == bank = size `div` len
                              | otherwise = size `div` len + amt + leftover
                   where leftover | size `mod` len >= (i - bank) `mod` len = 1
                                  | otherwise = 0
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
