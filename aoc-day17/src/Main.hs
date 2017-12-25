module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', scanl)

data Zipper a = Zipper ![a] !a ![a]

right :: Zipper a -> Zipper a
right (Zipper ls x []) = let (leftmost:more) = reverse (x : ls)
                         in Zipper [] leftmost more
right (Zipper ls y (x:xs)) = Zipper (y:ls) x xs

focus :: Zipper a -> a
focus (Zipper _ x _) = x

insertRight :: a -> Zipper a -> Zipper a
insertRight new (Zipper ls x rs) = Zipper (x : ls) new rs

fromList :: [a] -> Zipper a
fromList [] = error "empty zipper"
fromList (x:xs) = Zipper [] x xs

part1 :: Int -> Int
part1 stepSize = focus . right . foldl' step (fromList [0]) $ [1..2017]
  where step curr new = insertRight new . (!! stepSize) . iterate right $ curr

part2 :: Int -> Int
part2 stepSize = snd . last . filter ((== 0) . fst)
                 . scanl step (0,0) $ [1..50000000]
  where step (afterPosn, value) n = ((afterPosn + stepSize) `mod` n, n)

main :: IO ()
main = interact $ show . (part1 &&& part2) . read . head . lines
