module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Lazy as M
import Data.List (maximumBy, sort, nub)
import Data.Ord (comparing)

data Line = Line String Int [String] deriving Show
data Tree a = Node {nodeName :: String,
                    nodeWeight :: Int,
                    nodeSize :: Int,
                    nodeMeta :: a,
                    nodeChildren :: [Tree a]}
            deriving Show

parse :: [String] -> Line
parse (name:weight:more) = Line name (read weight) $ case more of
  [] -> []
  ("->":children) -> map (filter (/= ',')) children

buildTree :: [Line] -> M.Map String (Tree Int)
buildTree lines = t
  where t = M.fromList $ do
          (Line name weight children) <- lines
          let childNodes = map (t M.!) children
              size = 1 + sum (map nodeSize childNodes)
              totalWeight = weight + sum (map nodeMeta childNodes)
          return (name, Node name weight size totalWeight $ childNodes)

root :: M.Map String (Tree a) -> Tree a
root = maximumBy (comparing nodeSize) . M.elems

part1 :: Tree a -> String
part1 = nodeName

part2 :: Tree Int -> (String, Int)
part2 root@(Node _ w _ _ nodes) = let weights = sort . map nodeMeta $ nodes
                                      goalWeight = w + (length nodes *
                                                        (weights !! 1))
                             in interrogate root goalWeight

interrogate :: Tree Int -> Int -> (String, Int)
interrogate t@(Node n w _ tw children) goal
  | length (nub childWeights) <= 1 = (n, goal - sum childWeights)
  | otherwise = interrogate (head [child | child <- children, childGoal /= nodeMeta child]) childGoal
  where childWeights = map nodeMeta children
        childGoal = (goal - w) `div` length children

main :: IO ()
main = interact $ show . (part1 &&& part2) .
       root . buildTree . map (parse . words) . lines
