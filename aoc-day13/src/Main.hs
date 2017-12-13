module Main where

import Control.Arrow ((&&&))

import Data.Bool (bool)

type Layer = Int
type Range = Int
type Time = Int

data Firewall = Firewall Layer Range


caught :: Range -> Time -> Bool
caught r t = t `mod` ((r - 1) * 2) == 0

cost :: Firewall -> Int
cost (Firewall layer range) = layer * range

parse :: String -> Firewall
parse s = let (layer, (':':' ':range)) = break (== ':') s
          in Firewall (read layer) (read range)

alarms :: Int -> [Firewall] -> [Firewall]
alarms offset fws = [fw | fw@(Firewall layer range) <- fws,
                     caught range (layer + offset)]

part1 :: [Firewall] -> Int
part1 = sum . map cost . alarms 0

part2 :: [Firewall] -> Int
part2 fws = head [start | start <- [0..], null $ alarms start fws]

main :: IO ()
main = interact $ show . (part1 &&& part2) . map parse . lines
