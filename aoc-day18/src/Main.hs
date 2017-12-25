module Main where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token (integer)

type Parser a = Parsec String () a

type Register = Char
data Arg = Reg Register | Lit Int deriving Show
data Mutation = Set | Add | Mul | Mod deriving Show
data Instruction = Sound Arg
                 | Mutate Mutation Register Arg
                 | Recover Arg
                 | JumpPos Arg Arg
                 deriving (Show)

data Computer = Computer [Instruction] Int (M.Map Register Int) (Maybe Int)

mutate :: Mutation -> Int -> Int -> Int
mutate Set = flip const
mutate Add = (+)
mutate Mul = (*)
mutate Mod = mod

int :: Parser Int
int = do
  op <- option id $ char '-' *> pure negate
  op . read <$> many1 digit

arg :: Parser Arg
arg = Lit <$> int
  <|> Reg <$> anyChar

mut :: String -> Mutation -> Parser Instruction
mut s m = try $ string s *> spaces *>
  (Mutate m <$> anyChar <*> (spaces *> arg))

unary :: String -> (Arg -> Instruction) -> Parser Instruction
unary s f = try $ string s *> spaces *> (f <$> arg)

instruction :: Parser Instruction
instruction = mut "set" Set
          <|> mut "add" Add
          <|> mut "mul" Mul
          <|> mut "mod" Mod
          <|> unary "snd" Sound
          <|> unary "rcv" Recover
          <|> do
            string "jgz"
            spaces
            x <- arg
            spaces
            y <- arg
            pure $ JumpPos x y

program :: Parser [Instruction]
program = instruction `endBy1` newline <* eof

part1 = id

part2 :: a -> ()
part2 = const ()

parseProgram :: String -> [Instruction]
parseProgram s = case parse program "stdin" s of
  Left err -> error $ show err
  Right program -> program

main :: IO ()
main = interact $ show . (part1 &&& part2) . parseProgram
