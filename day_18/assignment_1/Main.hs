{- You discover a tablet containing some strange assembly code labeled simply
 - "Duet". Rather than bother the sound card with it, you decide to run the code
 - yourself. Unfortunately, you don't see any documentation, so you're left to
 - figure out what the instructions mean on your own.
 -
 - It seems like the assembly is meant to operate on a set of registers that are
 - each named with a single letter and that can each hold a single integer. You
 - suppose each register should start with a value of 0.
 -
 - There aren't that many instructions, so it shouldn't be hard to figure out
 - what they do. Here's what you determine:
 -
 -  * snd X plays a sound with a frequency equal to the value of X.
 -  * set X Y sets register X to the value of Y.
 -  * add X Y increases register X by the value of Y.
 -  * mul X Y sets register X to the result of multiplying the value contained
 -    in register X by the value of Y.
 -  * mod X Y sets register X to the remainder of dividing the value contained
 -    in register X by the value of Y (that is, it sets X to the result of X
 -    modulo Y).
 -  * rcv X recovers the frequency of the last sound played, but only when the
 -    value of X is not zero. (If it is zero, the command does nothing.)
 -  * jgz X Y jumps with an offset of the value of Y, but only if the value of X
 -    is greater than zero. (An offset of 2 skips the next instruction, an
 -    offset of -1 jumps to the previous instruction, and so on.)
 -
 - Many of the instructions can take either a register (a single letter) or a
 - number. The value of a register is the integer it contains; the value of a
 - number is that number.
 -
 - After each jump instruction, the program continues with the instruction to
 - which the jump jumped. After any other instruction, the program continues
 - with the next instruction. Continuing (or jumping) off either end of the
 - program terminates it.
 -
 - For example:
 -
 - set a 1
 - add a 2
 - mul a a
 - mod a 5
 - snd a
 - set a 0
 - rcv a
 - jgz a -1
 - set a 1
 - jgz a -2
 -
 -  * The first four instructions set a to 1, add 2 to it, square it, and then
 -    set it to itself modulo 5, resulting in a value of 4.
 -  * Then, a sound with frequency 4 (the value of a) is played.
 -  * After that, a is set to 0, causing the subsequent rcv and jgz instructions
 -    to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
 -  * Finally, a is set to 1, causing the next jgz instruction to activate,
 -    jumping back two instructions to another jump, which jumps again to the
 -    rcv, which ultimately triggers the recover operation.
 -
 - At the time the recover operation is executed, the frequency of the last
 - sound played is 4.
 -
 - What is the value of the recovered frequency (the value of the most recently
 - played sound) the first time a rcv instruction is executed with a non-zero
 - value? -}
module Main (main) where

import qualified Control.Monad.Loops as C
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

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
