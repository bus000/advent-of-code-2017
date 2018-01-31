{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad.Except as C
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

    print $ getMuls program

type Environment a b = Map a b
type ProgramCounter = Int
type InterpreterState = (ProgramCounter, Environment Char Integer)

{- Interpreter. -}

getMuls :: Program -> Either (String, Integer) Integer
getMuls program =
    case C.evalRWS (C.runExceptT (interpret program)) () (0, initEnv) of
        (Left err, asdf) -> Left (err, C.getSum asdf)
        (Right _, muls) -> Right . C.getSum $ muls
  where
    initEnv = Map.fromList $ zip ['a'..'h'] (repeat 0)

interpret :: (C.MonadRWS () (C.Sum Integer) InterpreterState m,
    C.MonadError String m) => Program -> m ()
interpret program = forever $ do
    (pc, env) <- C.get
    traceShowM pc
    when (pc > (V.length program - 1)) $ C.throwError "PC out of bounds"
    when (pc < 0 ) $ C.throwError "PC out of bounds"
    let stm = program V.! pc
    traceShowM stm
    C.put (pc+1, env)

    execStm stm
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
