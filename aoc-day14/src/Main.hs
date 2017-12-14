module Main where

import KnotHash (knot)
import Data.Bits
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Control.DeepSeq
import qualified Data.Map.Strict as M

import Control.Arrow ((&&&))

type Coord a = (a, a)
type Group = M.Map (Coord Int) Int

bits :: Int -> [Bool]
bits c = do
  bit <- [3,2..0]
  pure $ c .&. (1 `shiftL` bit) /= 0

row :: String -> Int -> [Bool]
row key idx = bits . digitToInt =<< knot (key ++ "-" ++ show idx)

grid :: Int -> String -> [[Bool]]
grid rows key = map (row key) [0..rows - 1]

coords :: [[Bool]] -> [Coord Int]
coords gs = do
  (y, row) <- zip [0..] gs
  (x, True) <- zip [0..] row
  pure (y, x)

asMap :: [Coord Int] -> Group
asMap = M.fromList . flip zip [0..]

neighbors :: Num a => Coord a -> [Coord a]
neighbors (y, x) = [(y + dy, x + dx) | (dy, dx) <- zip [0,0,1,-1] [1,-1,0,0]]

spread :: Coord Int -> Group -> Group
spread k m = m `deepseq` foldr relabel m (nub adjs)
  where adjs = catMaybes . map (`M.lookup` m) $ neighbors k
        new = m M.! k
        relabel old = fmap replace
          where replace x | x == old = new
                          | otherwise = x

coalesce :: Group -> Group
coalesce m = foldr spread m (M.keys m)

part1 :: [[Bool]] -> Int
part1 = length . filter id . concat

part2 :: [[Bool]] -> Int
part2 = length . nub . M.elems . coalesce . asMap . coords

main :: IO ()
main = interact $ show . (part1 &&& part2) . grid 128 . head . lines
