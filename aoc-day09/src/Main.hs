module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Lazy as M
import Control.Monad.Trans.State.Lazy

type Input = String

part2 :: String -> ()
part2 = const ()

part1 :: String -> Int
part1 = go 0
  where go depth ('{':more) = go (depth + 1) more
        go depth ('}':',':more) = go (depth - 1) more + depth
        go depth ('}':more) = go (depth - 1) more + depth
        go depth ('<':more) = garbage more
          where garbage ('!':_:more) = garbage more
                garbage ('>':',':more) = go depth more
                garbage ('>':more) = go depth more
                garbage (_:more) = garbage more
                garbage xs = error $ show (take 5 xs)
        go depth "" = depth
        go depth xs = error $ take 5 xs

main :: IO ()
main = interact $ show . (part1 &&& part2) . head . lines
