module Main where

import Prelude hiding (IO)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Maybe (catMaybes)
import Text.Parsec hiding (State)
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

data IO = S Arg | R Arg | Abort deriving Show

data Computer = Computer {instrs :: [Instruction],
                          ip :: Int,
                          regs :: M.Map Register Int}
                deriving (Show)

data State = Runnable | Blocked Register
data Process = Process State (S.Seq Int) Computer
data Supervisor = Supervisor Int Process Process

mkComputer :: Int -> [Instruction] -> Computer
mkComputer pid prog = Computer prog 0 $ M.fromList [('p', pid)]

mkProcess :: Int -> [Instruction] -> Process
mkProcess pid = Process Runnable S.empty . mkComputer pid

mutate :: Mutation -> Int -> Int -> Int
mutate Set = const
mutate Add = (+)
mutate Mul = (*)
mutate Mod = flip mod

eval :: Arg -> Computer -> Int
eval (Lit x) c = x
eval (Reg r) c = M.findWithDefault 0 r (regs c)

setReg :: Computer -> Register -> Int -> Computer
setReg c r x = c {regs = M.insert r x $ regs c}

runUntilIO :: Computer -> (Computer, IO)
runUntilIO c@(Computer prog ip regs)
  | ip < 0 || ip >= length prog = (c, Abort)
  | otherwise = let instr = prog !! ip
                    nextIP = ip + case instr of
                      JumpPos test offset | eval test c > 0 -> eval offset c
                      _ -> 1
                    state = case instr of
                      Mutate m r arg -> setReg c r (mutate m
                                                    (eval arg c)
                                                    (eval (Reg r) c))
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
          (c', Abort) -> error "aborted"

part2 :: [Instruction] -> Int
part2 instructions = go (Supervisor 0 <$> mkProcess 0
                                      <*> mkProcess 1
                                      $ instructions)
  where go (Supervisor numSends
            p1@(Process state1 inputs1 c1)
            p2@(Process state2 inputs2 c2)) =
          case (state1, S.viewl inputs1) of
            (Runnable, _) -> case runUntilIO c1 of
              (_, Abort) -> numSends
              (c1', S arg) -> go (Supervisor (numSends + 1)
                                  (Process Runnable inputs1 c1')
                                  (Process state2 (inputs2 S.|> eval arg c1') c2))
              (c1', R (Reg r)) -> go (Supervisor numSends
                                      (Process (Blocked r) inputs1 c1')
                                      p2)
            (Blocked _, S.EmptyL) -> runP2
            (Blocked r, (x S.:< more)) -> go (Supervisor numSends
                                              (Process Runnable more $ setReg c1 r x)
                                              p2)
          where runP2 = case (state2, S.viewl inputs2) of
                  (Runnable, _) -> case runUntilIO c2 of
                    (_, Abort) ->  numSends
                    (c2', S arg) -> go (Supervisor numSends
                                        (Process state1 (inputs1 S.|> eval arg c2') c2')
                                        (Process Runnable inputs2 c2'))
                    (c2', R (Reg r)) -> go (Supervisor numSends p1
                                            (Process (Blocked r) inputs2 c2'))
                  (Blocked _, S.EmptyL) -> numSends
                  (Blocked r, (x S.:< more)) -> go (Supervisor numSends p1
                                                    (Process Runnable more $ setReg c2 r x))


parseProgram :: String -> [Instruction]
parseProgram s = case parse program "stdin" s of
  Left err -> error $ show err
  Right program -> program

main = interact $ show . (part1 &&& part2) . parseProgram
