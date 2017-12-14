module KnotHash
    ( knot
    ) where

import Data.Bits (xor)
import Data.Char (intToDigit, ord)
import Data.List (foldl')

data LS a = LS {_s :: [a],
                _len :: Int}
            deriving Show

data State = State {_string :: LS Int,
                    _index :: Int,
                    _skip :: Int}
             deriving Show

mkState :: Int -> State
mkState len = State (LS [0..len - 1] len) 0 0

twist :: Int -> Int -> LS a -> LS a
twist start len (LS xs strLen) =
  let loop = drop start . cycle $ xs
      flipped = take strLen $ reverse (take len loop) ++ drop len loop
  in LS (take strLen . drop (strLen - start) . cycle $ flipped) strLen

step :: State -> Int -> State
step (State s@(LS xs strLen) offset skip) len =
  State (twist offset len s) ((offset + len + skip) `mod` strLen) (skip + 1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

hex :: Int -> String
hex x = [intToDigit (x `div` 16), intToDigit (x `mod` 16)]

knot :: String -> String
knot input =
  let lengths = map ord input ++ [17, 31, 73, 47, 23]
      rounds = concat . replicate 64 $ lengths
      sparseHash = foldl step (mkState 256) rounds
      denseHash = map (foldl' xor 0) . chunksOf 16 . _s . _string $ sparseHash
  in denseHash >>= hex
