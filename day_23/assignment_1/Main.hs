{- You decide to head directly to the CPU and fix the printer from there. As you
 - get close, you find an experimental coprocessor doing so much work that the
 - local programs are afraid it will halt and catch fire. This would cause
 - serious issues for the rest of the computer, so you head in and see what you
 - can do.
 -
 - The code it's running seems to be a variant of the kind you saw recently on
 - that tablet. The general functionality seems very similar, but some of the
 - instructions are different:
 -
 -  * set X Y sets register X to the value of Y.
 -  * sub X Y decreases register X by the value of Y.
 -  * mul X Y sets register X to the result of multiplying the value contained
 -    in register X by the value of Y.
 -  * jnz X Y jumps with an offset of the value of Y, but only if the value of X
 -    is not zero. (An offset of 2 skips the next instruction, an offset of -1
 -    jumps to the previous instruction, and so on.)
 -
 - Only the instructions listed above are used. The eight registers here, named a
 - through h, all start at 0.
 -
 - The coprocessor is currently set to some kind of debug mode, which allows for
 - testing, but prevents it from doing any meaningful work.
 -
 - If you run the program (your puzzle input), how many times is the mul
 - instruction invoked? -}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad.Except as C
import qualified Control.Monad.Loops as C
import qualified Control.Monad.RWS.Strict as C
import qualified Data.Map as Map
import qualified Data.Vector as V
import Prelude ()
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

type Register = Char
data Value = RegVal !Register | Value !Integer deriving (Show)
data Stm
    = Set !Register !Value
    | Jump !Value !Integer
    | Mul !Register !Value
    | Sub !Register !Value
  deriving (Show)
type Program = V.Vector Stm

main :: IO ()
main = do
    program <- either (Sys.die . show) pure =<< parseInput <$> getContents

    either Sys.die print $ getMuls program

type Environment a b = Map a b
type ProgramCounter = Int
type InterpreterState = (ProgramCounter, Environment Char Integer)

{- Interpreter. -}

getMuls :: Program -> Either String Integer
getMuls program =
    case C.evalRWS (C.runExceptT (interpret program)) () (0, initEnv) of
        (Left err, _) -> Left err
        (Right _, muls) -> Right . C.getSum $ muls
  where
    initEnv = Map.fromList $ zip ['a'..'h'] (repeat 0)

interpret :: (C.MonadRWS () (C.Sum Integer) InterpreterState m,
    C.MonadError String m) => Program -> m ()
interpret program = C.whileJust_ getStm execStm
  where
    execStm (Set register value) = do
        (pc, env) <- C.get

        when ((== Nothing) $ Map.lookup register env) $
            C.throwError "Unknown Register"

        value' <- computeValue value
        let env' = Map.insert register value' env

        C.put (pc, env')
    execStm (Jump value int) = do
        (pc, env) <- C.get

        value' <- computeValue value

        when (value' /= 0) $ C.put (pc + (fromIntegral int - 1), env)
    execStm (Mul register value) = do
        (pc, env) <- C.get

        C.tell 1 -- Report that we did a multiplication.

        a <- computeValue value
        b <- maybe (C.throwError "Unknown Register") pure $ lookup register env

        let env' = insertMap register (a * b) env

        C.put (pc, env')
    execStm (Sub register value) = do
        (pc, env) <- C.get

        a <- maybe (C.throwError "Unknown Register") pure $ lookup register env
        b <- computeValue value

        let env' = insertMap register (a - b) env

        C.put (pc, env')

    computeValue (RegVal register) = do
        env <- C.gets snd

        maybe (C.throwError "Unknown Register") pure $ Map.lookup register env
    computeValue (Value int) = return int

    getStm = do
        (pc, env) <- C.get
        if pc < 0 || pc > (V.length program - 1)
        then return Nothing
        else do
            C.put (pc+1, env)
            return $ Just (program V.! pc)

{- Parser. -}

parseInput :: LText -> Either P.ParseError Program
parseInput txt = V.fromList <$> P.parse (statementsParser <* P.eof) "" txt

type Parser a = P.Parsec LText () a

statementsParser :: Parser [Stm]
statementsParser = statementParser `P.endBy` P.newline

statementParser :: Parser Stm
statementParser = P.choice $ map P.try
    [ setParser
    , jumpParser
    , mulParser
    , subParser
    ]
  where
    setParser = Set
        <$> (P.string "set " *> registerParser)
        <*> (P.char ' ' *> valueParser)
    jumpParser = Jump
        <$> (P.string "jnz " *> valueParser)
        <*> (P.char ' ' *> P.int)
    mulParser = Mul
        <$> (P.string "mul " *> registerParser)
        <*> (P.char ' ' *> valueParser)
    subParser = Sub
        <$> (P.string "sub " *> registerParser)
        <*> (P.char ' ' *> valueParser)

registerParser :: Parser Register
registerParser = P.oneOf ['a'..'z']

valueParser :: Parser Value
valueParser = P.choice [Value <$> P.int, RegVal <$> registerParser]
