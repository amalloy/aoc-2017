{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Main where

import Data.List (maximumBy)
import Data.Ord (comparing, Down(..))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, char, string, spaces, digit)

import Control.Arrow ((&&&))

data Vector a = Vector a a a deriving (Show, Functor, Foldable)
data Particle a = Particle {acceleration, velocity, position :: Vector a} deriving Show

direction :: Num a => Vector a -> Vector a
direction = fmap signum

magnitude :: (Num a, Eq a) => Vector a -> Vector a -> a
magnitude (Vector ux uy uz) (Vector x y z) = sum (zipWith along [x, y, z] [ux, uy, uz])
  where n `along` 0 = abs n
        n `along` sign = n * sign

tieBreakers :: (Num a, Eq a) => Particle a -> [a]
tieBreakers (Particle a v p) = map (magnitude (direction a)) [a, v, p]

field :: Parser [Particle Int]
field = particle `P.endBy` newline

particle :: Parser (Particle Int)
particle = do
  p <- vector 'p' <* delim
  v <- vector 'v' <* delim
  a <- vector 'a'
  pure $ Particle a v p
  where delim = char ',' *> spaces

vector :: Char -> Parser (Vector Int)
vector label = do
  string $ label : "=<"
  x <- int <* char ','
  y <- int <* char ','
  z <- int <* char '>'
  pure $ Vector x y z

int :: Parser Int
int = do
  sign <- P.option id $ char '-' *> pure negate
  sign . read <$> P.many1 digit

parse :: String -> [Particle Int]
parse s = case P.parse field "" s of
  Left e -> error $ show e
  Right field -> field

part1 :: [Particle Int] -> Int
part1 = fst . maximumBy (comparing (Down . tieBreakers . snd)) . zip [0..]

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
