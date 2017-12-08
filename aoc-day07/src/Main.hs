module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Lazy as M
import Data.List (maximumBy, sort, nub)
import Data.Ord (comparing)

data Line = Line String Int [String] deriving Show
data Tree = Node {_name :: String,
                  _weight :: Int,
                  _size :: Int,
                  _totalWeight :: Int,
                  _children :: [Tree]}
            deriving Show

parse :: [String] -> Line
parse (name:weight:more) = Line name (read weight) $ case more of
  [] -> []
  ("->":children) -> map (filter (/= ',')) children

buildTree :: [Line] -> M.Map String Tree
buildTree lines = t
  where t = M.fromList $ do
          (Line name weight children) <- lines
          let childNodes = map (t M.!) children
              size = 1 + sum (map _size childNodes)
              totalWeight = weight + sum (map _totalWeight childNodes)
          return (name, Node name weight size totalWeight childNodes)

root :: M.Map String Tree -> Tree
root = maximumBy (comparing _size) . M.elems

interrogate :: Int -> Tree -> (String, Int)
interrogate goal t@(Node n w _ tw children)
  | length (nub childWeights) <= 1 = (n, goal - sum childWeights)
  | otherwise = interrogate childGoal
                            (head [child | child <- children,
                                   childGoal /= _totalWeight child])
  where childWeights = map _totalWeight children
        childGoal = (goal - w) `div` length children

part1 :: Tree -> String
part1 = _name

part2 :: Tree -> (String, Int)
part2 root@(Node _ w _ _ nodes) = let weights = sort . map _totalWeight $ nodes
                                      goalWeight = w + (length nodes *
                                                        (weights !! 1))
                                  in interrogate goalWeight root

main :: IO ()
main = interact $ show . (part1 &&& part2) .
       root . buildTree . map (parse . words) . lines
