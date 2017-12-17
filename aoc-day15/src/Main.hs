module Main where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Bits ((.&.))

type Gens = (Integer, Integer)

next :: Integer -> Integer -> Integer
next curr factor = (factor * curr) `mod` 2147483647

part1 :: Gens -> Int
part1 (a, b) = length . filter id . take 40000000 . tail $ zipWith ((==) `on` (.&. 0xffff)) (iterate (next 16807) a) (iterate (next 48271) b)

part2 :: a -> ()
part2 = const ()

parse :: String -> Integer
parse = read . last . words

puzzle :: [String] -> Gens
puzzle [a,b] = (parse a, parse b)

main :: IO ()
main = interact $ show . (part1 &&& part2) . puzzle . lines
