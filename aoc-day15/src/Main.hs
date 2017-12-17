module Main where

import Control.Arrow ((&&&))
import Data.Function (on)

type States = ([Integer], [Integer])

next :: Integer -> Integer -> Integer
next seed curr = (seed * curr) `mod` 2147483647

vals :: Integer -> Integer -> [Integer]
vals = iterate . next
-- vals factor init = iterate (next factor) init
-- vals factor = iterate (next factor)

part1 :: States -> Int
part1 = length . filter id . take 40000000 . uncurry (zipWith ((==) `on` (`mod` 65536)))

part2 :: a -> ()
part2 = const ()

parse :: String -> Integer
parse = read . last . words

puzzle :: [String] -> States
puzzle [a,b] = (tail . vals 16807 $ parse a, tail . vals 48271 $ parse b)

main :: IO ()
main = interact $ show . (part1 &&& part2) . puzzle . lines
