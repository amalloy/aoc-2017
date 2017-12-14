module Main where

import Control.Arrow ((&&&))

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import Control.Monad (when)
import Data.List (nub)


type Program = Int
data ProgInfo = PI {parent :: Program,
                    rank :: Int} deriving Show
type Directory = M.Map Program ProgInfo
type Lookup a = State Directory a


mkSet :: Program -> Lookup ProgInfo
mkSet p = do
  modify $ M.insertWith (flip const) p (PI p 0)
  gets (M.! p)

find :: Program -> Lookup ProgInfo
find p = do
  pi@(PI parent rank) <- mkSet p
  if parent == p
    then pure pi
    else do
      root <- find parent
      modify $ M.insert p root
      pure root

union :: Program -> Program -> Lookup ()
union x y = do
  xn@(PI xRoot xRank) <- find x
  yn@(PI yRoot yRank) <- find y
  when (xRoot /= yRoot) $
    modify $ case compare xRank yRank of
      LT -> M.insert xRoot yn
      GT -> M.insert yRoot xn
      EQ -> M.union $ M.fromList [(yRoot, xn),
                                  (xRoot, (PI xRoot (succ xRank)))]

parse :: [String] -> [Program]
parse (p:"<->":progs) = read <$> p : map (filter (/= ',')) progs

flatten :: [[Program]] -> Directory
flatten neighbors = execState go M.empty
  where go = do
          mapM_ process neighbors
          mapM_ find =<< gets M.keys
        process (x:xs) = mapM_ (union x) xs

part1 :: Directory -> Int
part1 m = length . filter ((== goal) . parent) . M.elems $ m
  where (PI goal _) = m M.! 0

part2 :: Directory -> Int
part2 = length . nub . map parent . M.elems

main :: IO ()
main = interact $ show . (part1 &&& part2) . flatten . map (parse . words) . lines
