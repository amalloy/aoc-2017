module Main where
import Data.Char (digitToInt)

pairs :: String -> [(Char, Char)]
pairs s = zip s (tail s ++ [head s])

part1 :: [(Char, Char)] -> Int
part1 ps = sum [digitToInt a | (a, b) <- ps, a == b]

main :: IO ()
main = interact (show . part1 . pairs . filter (/= '\n'))
