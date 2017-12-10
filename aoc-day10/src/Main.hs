module Main where

import Control.Arrow ((&&&))

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

part1 = answer . foldl step initState
  where answer = product . take 2 . _s . _string
        initState = mkState 256

part2 :: a -> ()
part2 = const ()

parse :: String -> [Int]
parse = fmap read . split
  where split s = case break (== ',') s of
          (num, (',':more)) -> num : split more
          (num, "") -> [num]

main :: IO ()
main = interact $ show . (part1 &&& part2) .  parse . head . lines
