module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Lazy as M
import Control.Monad.Trans.State.Lazy

type Input = String

solve :: String -> Int
solve = go 0
  where go depth ('{':more) = go (depth + 1) more
        go depth ('}':',':more) = go (depth - 1) more
        go depth ('}':more) = go (depth - 1) more
        go depth ('<':more) = garbage more
          where garbage ('!':_:more) = garbage more
                garbage ('>':',':more) = go depth more
                garbage ('>':more) = go depth more
                garbage (_:more) = garbage more + 1
                garbage xs = error $ show (take 5 xs)
        go depth "" = depth
        go depth xs = error $ take 5 xs

main :: IO ()
main = interact $ show . solve . head . lines
