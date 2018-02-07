module Main where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, char, string, spaces, digit)

import Control.Arrow ((&&&))

data Vector a = Vector a a a deriving Show
data Particle a = Particle {acceleration, velocity, position :: Vector a} deriving Show

field :: Parser [Particle Int]
field = particle `P.endBy` newline

particle :: Parser (Particle Int)
particle = do
  p <- vector 'p'
  delim
  v <- vector 'v'
  delim
  a <- vector 'a'
  pure $ Particle a v p
  where delim = char ',' *> spaces

vector :: Char -> Parser (Vector Int)
vector label = do
  string $ label : "=<"
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  char '>'
  pure $ Vector x y z

int :: Parser Int
int = do
  sign <- P.option id $ char '-' *> pure negate
  sign . read <$> P.many1 digit

parse :: String -> [Particle Int]
parse s = case P.parse field "" s of
  Left e -> error $ show e
  Right field -> field

-- todo: collapse each particle into 1d vectors in the direction of acceleration
-- comparing those by tiebreakers should work

-- part1 :: [Particle Int] -> ()
part1 = id

part2 :: a -> ()
part2 = const ()

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
