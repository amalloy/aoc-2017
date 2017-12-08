module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Lazy as M
import Data.List (maximumBy)
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

buildTree :: [Line] -> M.Map String (Tree ())
buildTree lines = t
  where t = M.fromList $ do
          (Line name weight children) <- lines
          let childNodes = map (t M.!) children
              size = 1 + sum (map nodeSize childNodes)
          return (name, Node name weight size () $ childNodes)

root :: M.Map String (Tree a) -> Tree a
root = maximumBy (comparing nodeSize) . M.elems

part1 :: Tree a -> String
part1 = nodeName

part2 :: Tree a -> Int
part2 t = undefined

main :: IO ()
main = interact $ show . (part1 &&& part2) .
       root . buildTree . map (parse . words) . lines
