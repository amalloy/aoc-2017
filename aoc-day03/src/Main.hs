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

spiral :: State MapGrid Int -> State MapGrid (Maybe a) -> State MapGrid a
spiral label result = go
  where go = do
          newValue <- label
          (Grid i dir pos m) <- get
          put $ Grid i dir pos (M.insert pos newValue m)
          done <- result
          case done of
            Just x -> return x
            Nothing -> do
              let turned = turnLeft dir
                  left = move turned pos
                  forward = move dir pos
                  (dir', pos') = if M.member left m
                                 then (dir, forward)
                                 else (turned, left)
              put $ Grid (i + 1) dir' pos' (M.insert pos i m)
              go

solve :: State MapGrid Int -> State MapGrid (Maybe a) -> a
solve label result = evalState (spiral label result)
                     (Grid 1 Down (Pos 0 0) M.empty)

part1 :: Int -> Int
part1 goal = solve label result
  where label = do
          (Grid i _ _ _) <- get
          return i
        result = do
          (Grid i _ (Pos y x) _) <- get
          if i == goal
            then return $ Just (abs x + abs y)
            else return Nothing

main :: IO ()
main = interact $ show . part1 . read
