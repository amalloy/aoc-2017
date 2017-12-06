module Main where

import Prelude hiding (Left, Right)

import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as M

data Direction = Left | Up | Right | Down
data Position = Pos Int Int deriving (Ord, Eq)
data MapGrid = Grid Int Direction Position (M.Map Position Int)

turnLeft :: Direction -> Direction
turnLeft Left = Down
turnLeft Up = Left
turnLeft Right = Up
turnLeft Down = Right

move :: Direction -> Position -> Position
move dir (Pos y x) = case dir of
  Left -> Pos y (x - 1)
  Up -> Pos (y + 1) x
  Right -> Pos y (x + 1)
  Down -> Pos (y - 1) x

spiral :: Int -> State MapGrid Position
spiral goal = do
  (Grid i dir pos m) <- get
  if i == goal
    then return pos
    else do
      let turned = turnLeft dir
          left = move turned pos
          forward = move dir pos
          (dir', pos') = if M.member left m
                         then (dir, forward)
                         else (turned, left)
      put $ Grid (i + 1) dir' pos' (M.insert pos i m)
      spiral goal

part1 :: Int -> Int
part1 goal = let (Pos y x) = evalState (spiral goal)
                             (Grid 1 Down (Pos 0 0) M.empty)
             in abs y + abs x

main :: IO ()
main = interact $ show . part1 . read
