module Main where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Bits ((.&.))

type Gens = (Integer, Integer)

next :: Integer -> Integer -> Integer
next factor curr = (factor * curr) `mod` 2147483647

judge :: [Integer] -> [Integer] -> [Bool]
judge = zipWith ((==) `on` (.&. 0xffff))

part1 :: Gens -> Int
part1 (a, b) = length . filter id . take 40000000 . tail $ judge (iterate (next 16807) a) (iterate (next 48271) b)

part2 :: Gens -> Int
part2 (a, b) = length . filter id . take 5000000 $ judge as bs
  where as = go 16807 4 a
        bs = go 48271 8 b
        go factor rem = filter ((== 0) . (`mod` rem)) . iterate (next factor)

parse :: String -> Integer
parse = read . last . words

puzzle :: [String] -> Gens
puzzle [a,b] = (parse a, parse b)

main :: IO ()
main = interact $ show . (part1 &&& part2) . puzzle . lines
