module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M

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

dance :: [Op] -> String -> String
dance = flip (foldl apply)

shortcut :: Ord a => (a -> a) -> Int -> a -> a
shortcut f = go M.empty
  where go m 0 x = x
        go m n x = case M.lookup x m of
          Nothing -> go (M.insert x n m) (n - 1) (f x)
          Just prev -> shortcut f (n `mod` (prev - n)) x

part1 :: [Op] -> String
part1 = flip dance ['a'..'p']

part2 :: [Op] -> String
part2 ops = shortcut (dance ops) 1000000000 ['a'..'p']

main :: IO ()
main = interact $ show . (part1 &&& part2) . map parse . split . head . lines
