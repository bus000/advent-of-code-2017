module Main (main) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map
import qualified Control.Monad.State as S
import qualified Control.Monad.Loops as C
import qualified System.Exit as Sys
import qualified Data.Vector as V
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right program -> print . head . interpret $ V.fromList program

{- Interpreter. -}
type PC = Int -- Program Counter.
type Interpreter a = S.State (Map.Map Register Int, PC, [PlayedNote]) a
type PlayedNote = Int

interpret :: V.Vector Stm -> [PlayedNote]
interpret program = Maybe.catMaybes playedNotes
  where
    playedNotes = S.evalState (runCurrent program `C.untilM` outside)
        initialState
    initialState = (Map.empty, 0, [])
    outside = do
        (_, pc, _) <- S.get
        return $ pc < 0 || pc >= V.length program

runCurrent :: V.Vector Stm -> Interpreter (Maybe PlayedNote)
runCurrent program = do
    (_, pc, _) <- S.get
    let instruction = program V.! pc
    runStm instruction

runStm :: Stm -> Interpreter (Maybe PlayedNote)
runStm (Sound value) = do
    (env, pc, frequencies) <- S.get

    frequency <- getValue value

    S.put (env, pc + 1, frequency:frequencies)
    return Nothing
runStm (Set register value) = do
    (env, pc, frequencies) <- S.get

    v <- getValue value

    S.put (Map.insert register v env, pc + 1, frequencies)
    return Nothing
runStm (Add register value) = do
    (env, pc, frequencies) <- S.get

    v <- getValue value

    S.put (Map.insertWith (+) register v env, pc + 1, frequencies)
    return Nothing
runStm (Mul register value) = do
    (env, pc, frequencies) <- S.get

    v <- getValue value

    let val = Map.findWithDefault 0 register env * v

    S.put (Map.insert register val env, pc + 1, frequencies)
    return Nothing
runStm (Mod register value) = do
    (env, pc, frequencies) <- S.get

    v <- getValue value

    let val = Map.findWithDefault 0 register env `mod` v

    S.put (Map.insert register val env, pc + 1, frequencies)
    return Nothing
runStm (Record register) = do
    (env, pc, frequencies) <- S.get
    S.put (env, pc + 1, frequencies)

    if Map.findWithDefault 0 register env /= 0
        then return $ Maybe.listToMaybe frequencies
        else return Nothing
runStm (Jump value1 value2) = do
    (env, pc, frequencies) <- S.get

    shouldJump <- ifValue value1
    jumpLen <- getValue value2

    if shouldJump
        then S.put (env, pc + jumpLen, frequencies) >> return Nothing
        else S.put (env, pc + 1, frequencies) >> return Nothing

getValue :: Value -> Interpreter Int
getValue (Literal n) = return n
getValue (RegisterValue register) = do
    (env, _, _) <- S.get
    return $ Map.findWithDefault 0 register env

ifValue :: Value -> Interpreter Bool
ifValue (Literal n) = return $ n /= 0
ifValue register = (/= 0) <$> getValue register

{- Declare and parse statements. -}
type Register = Char

data Value = RegisterValue Register | Literal Int deriving (Show)

data Stm
    = Sound Value
    | Set Register Value
    | Add Register Value
    | Mul Register Value
    | Mod Register Value
    | Record Register
    | Jump Value Value
  deriving (Show)

parseInput :: String -> Either P.ParseError [Stm]
parseInput = P.parse (statements <* P.eof) ""

statements :: P.Parsec String () [Stm]
statements = statement `P.endBy` P.char '\n'

statement :: P.Parsec String () Stm
statement = P.choice $ map P.try
    [ soundP
    , setP
    , addP
    , mulP
    , modP
    , recordP
    , jumpP
    ]

soundP :: P.Parsec String () Stm
soundP = Sound <$> (P.string "snd " *> valueP)

setP :: P.Parsec String () Stm
setP = Set <$> (P.string "set " *> registerP <* P.char ' ') <*> valueP

addP :: P.Parsec String () Stm
addP = Add <$> (P.string "add " *> registerP <* P.char ' ') <*> valueP

mulP :: P.Parsec String () Stm
mulP = Mul <$> (P.string "mul " *> registerP <* P.char ' ') <*> valueP

modP :: P.Parsec String () Stm
modP = Mod <$> (P.string "mod " *> registerP <* P.char ' ') <*> valueP

recordP :: P.Parsec String () Stm
recordP = Record <$> (P.string "rcv " *> registerP)

jumpP :: P.Parsec String () Stm
jumpP = Jump <$> (P.string "jgz " *> valueP <* P.char ' ') <*> valueP

registerP :: P.Parsec String () Register
registerP = P.oneOf ['a'..'z']

valueP :: P.Parsec String () Value
valueP = P.choice [literal, registerValue]
  where
    literal = Literal <$> P.int
    registerValue = RegisterValue <$> registerP
