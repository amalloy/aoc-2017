module Main where

import Control.Arrow ((&&&))

import Data.Bool (bool)

type Layer = Int
type Range = Int
type Time = Int

data Firewall = Firewall Layer Range


caught :: Range -> Time -> Bool
caught r t = t `mod` ((r - 1) * 2) == 0

cost :: Firewall -> Time -> Int
cost (Firewall layer range) t | caught range t = (layer * range)
                              | otherwise = 0

parse :: String -> Firewall
parse s = let (layer, (':':' ':range)) = break (== ':') s
          in Firewall (read layer) (read range)

part1 :: [Firewall] -> Int
part1 fws = sum [cost fw layer |
                 fw@(Firewall layer range) <- fws,
                 caught range layer]

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . map parse . lines
