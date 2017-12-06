module Main where
import Control.Arrow ((&&&))
import Data.Char (digitToInt)

part1 :: String -> [(Char, Char)]
part1 s = zip s (tail s ++ [head s])

part2 :: String -> [(Char, Char)]
part2 s = zip s (drop (length s `div` 2) (cycle s))

solve :: [(Char, Char)] -> Int
solve ps = sum [digitToInt a | (a, b) <- ps, a == b]

main :: IO ()
main = interact (show . ((solve . part1) &&& (solve . part2))
                 . filter (/= '\n'))
