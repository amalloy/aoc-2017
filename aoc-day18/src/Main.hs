module Main where

import Prelude hiding (IO)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token (integer)

type Parser a = Parsec String () a

type Register = Char
data Arg = Reg Register | Lit Int deriving Show
data Mutation = Set | Add | Mul | Mod deriving Show
data Instruction = Mutate Mutation Register Arg
                 | Snd Arg
                 | Rcv Arg
                 | JumpPos Arg Arg
                 deriving (Show)

data IO = S Arg | R Arg deriving Show

data Computer = Computer {instrs :: [Instruction],
                          ip :: Int,
                          regs :: M.Map Register Int}
                deriving (Show)

mutate :: Mutation -> Int -> Int -> Int
mutate Set = const
mutate Add = (+)
mutate Mul = (*)
mutate Mod = flip mod

eval :: Arg -> Computer -> Int
eval (Lit x) c = x
eval (Reg r) c = M.findWithDefault 0 r (regs c)

runUntilIO :: Computer -> (Computer, IO)
runUntilIO c@(Computer prog ip regs) =
  let instr = prog !! ip
      nextIP = ip + case instr of
        JumpPos test offset | eval test c > 0 -> eval offset c
        _ -> 1
      state = case instr of
        Mutate m r arg -> c {regs = M.insert r (mutate m
                                                (eval arg c)
                                                (eval (Reg r) c))
                                    regs}
        _ -> c
      state' = state {ip = nextIP}
  in case instr of
    Snd arg -> (state', S arg)
    Rcv arg -> (state', R arg)
    _ -> runUntilIO state'

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
          <|> unary "snd" Snd
          <|> unary "rcv" Rcv
          <|> do
            string "jgz"
            spaces
            x <- arg
            spaces
            y <- arg
            pure $ JumpPos x y

program :: Parser [Instruction]
program = instruction `endBy1` newline <* eof

-- Less cluttered output, for visual debugging
debug c = (instrs c !! ip c, regs c)

part1 instructions = go Nothing $ Computer instructions 0 M.empty
  where go sound c = case runUntilIO c of
          (c', io@(S arg)) -> go (Just (eval arg c')) c'
          (c', io@(R arg)) | eval arg c' /= 0 -> sound
                           | otherwise -> go sound c'

part2 :: a -> ()
part2 = const ()

parseProgram :: String -> [Instruction]
parseProgram s = case parse program "stdin" s of
  Left err -> error $ show err
  Right program -> program

main = interact $ show . (part1 &&& part2) . parseProgram
