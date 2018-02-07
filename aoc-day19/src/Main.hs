{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import Data.Array
import qualified Control.Monad.Trans.Writer.Lazy as W

newtype Coord a = Coord (a, a) deriving (Show, Ix, Ord, Eq)
newtype Dir a = Dir (a, a) deriving Show
data Loc label = Die | Turn | Straight (Maybe label) deriving (Show, Eq)
type Diagram = Array (Coord Int) (Loc Char)
data State = State (Dir Int) (Coord Int) deriving Show
type Path = [(Coord Int, Loc Char)]

loc :: Char -> Loc Char
loc ' ' = Die
loc '+' = Turn
loc c = Straight label
  where label | c `elem` "|-" = Nothing
              | otherwise = Just c

parse :: [String] -> Diagram
parse lines = array (Coord (0, 0), Coord (h, w)) $ do
  (y, row) <- zip [0..] lines
  (x, c) <- zip [0..] row
  pure (Coord (y, x), loc c)
  where h = length lines - 1
        w = length (head lines) - 1

newDirs :: Loc a -> Dir Int -> [Dir Int]
newDirs Die _ = []
newDirs (Straight _) x = [x]
newDirs Turn from = Dir <$> case from of
  Dir (0, _) -> [(1, 0), (-1, 0)]
  Dir (_, 0) -> [(0, 1), (0, -1)]

move :: Num a => Coord a -> Dir a -> Coord a
move (Coord (y, x)) (Dir (dy, dx)) = Coord (y + dy, x + dx)

follow :: Diagram -> State -> W.Writer [(Coord Int)] ()
follow grid = go
  where go (State dir pos) = case grid ! pos of
          Die -> pure ()
          here -> W.tell [pos] >> mapM_ go [State d (move pos d) | d <- newDirs here dir]


initState :: Diagram -> State
initState g = head [State (Dir (1, 0)) pos | (pos, (Straight _)) <- assocs g]

path :: Diagram -> Path
path grid = let visited = W.execWriter (follow grid (initState grid))
            in [(pos, grid ! pos) | pos <- visited]

part1 :: Path -> String
part1 path = [label | (_, Straight (Just label)) <- path]

part2 :: Path -> Int
part2 = length

main :: IO ()
main = interact $ show . (part1 &&& part2) . path . parse . lines
