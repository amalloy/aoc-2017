module Main where

import Control.Arrow ((&&&))

data Op = Spin Int | Exchange Int Int | Partner Char Char

parse :: String -> Op
parse ('s':more) = Spin $ read more
parse (op:more) = case op of
    'x' -> Exchange (read before) (read after)
    'p' -> Partner (head before) (head after)
  where (before, ('/':after)) = break (== '/') more

apply :: String -> Op -> String
apply s (Spin n) = drop amt s ++ take amt s
  where amt = length s - n
apply s (Exchange a b) = zipWith exchange [0..] s
  where exchange i c | i == a = s !! b
                     | i == b = s !! a
                     | otherwise = c
apply s (Partner a b) = map partner s
  where partner c | c == a = b
                  | c == b = a
                  | otherwise = c

split :: String -> [String]
split "" = []
split s = case break (== ',') s of
  (before, ',':after) -> before : split after
  (before, "") -> [before]

states :: [Op] -> [String]
states = scanl apply ['a'..'p']

part1 :: [String] -> String
part1 = last

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . states . map parse . split . head . lines
