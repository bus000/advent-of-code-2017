{- As you congratulate yourself for a job well done, you notice that the
 - documentation has been on the back of the tablet this entire time. While you
 - actually got most of the instructions correct, there are a few key
 - differences.  This assembly code isn't about sound at all - it's meant to be
 - run twice at the same time.
 -
 - Each running copy of the program has its own set of registers and follows
 - the code independently - in fact, the programs don't even necessarily run
 - at the same speed. To coordinate, they use the send (snd) and receive (rcv)
 - instructions:
 -
 -  * snd X sends the value of X to the other program. These values wait in a
 -    queue until that program is ready to receive them. Each program has its
 -    own message queue, so a program can never receive a message it sent.
 -  * rcv X receives the next value and stores it in register X. If no values
 -    are in the queue, the program waits for a value to be sent to it. Programs
 -    do not continue to the next instruction until they have received a value.
 -    Values are received in the order they are sent.
 -
 - Each program also has its own program ID (one 0 and the other 1); the
 - register p should begin with this value.
 -
 - For example:
 -
 - snd 1
 - snd 2
 - snd p
 - rcv a
 - rcv b
 - rcv c
 - rcv d
 -
 - Both programs begin by sending three values to the other. Program 0 sends 1,
 - 2, 0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1)
 - and stores it in a, receives another value (both 2) and stores it in b, and
 - then each receives the program ID of the other program (program 0 receives 1;
 - program 1 receives 0) and stores it in c. Each program now sees a different
 - value in its own copy of register c.
 -
 - Finally, both programs try to rcv a fourth time, but no data is waiting for
 - either of them, and they reach a deadlock. When this happens, both programs
 - terminate.
 -
 - It should be noted that it would be equally valid for the programs to run at
 - different speeds; for example, program 0 might have sent all three values
 - and then stopped at the first rcv before program 1 executed even its first
 - instruction.
 -
 - Once both of your programs have terminated (regardless of what caused them to
 - do so), how many times did program 1 send a value? -}
module Main (main) where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as C
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right program -> interpret $ V.fromList program

{- Interpreter. -}
data InterpreterState = InterpreterState
    { intEnv            :: Map.Map Register Integer
    , intProgramCounter :: Int
    , intReadCount      :: STM.TVar Integer
    , intSendCount      :: STM.TVar Integer
    , intReceive        :: STM.TChan Integer
    , intSend           :: STM.TChan Integer
    }

type Interpreter a = S.StateT InterpreterState IO a

interpret :: V.Vector Stm -> IO ()
interpret program = do
    readC1 <- STM.newTVarIO 0
    readC2 <- STM.newTVarIO 0
    sendC1 <- STM.newTVarIO 0
    sendC2 <- STM.newTVarIO 0
    chan1 <- STM.newTChanIO
    chan2 <- STM.newTChanIO

    let env1 = Map.singleton 'p' 0
        env2 = Map.singleton 'p' 1
        initialState1 = InterpreterState env1 0 readC1 sendC1 chan1 chan2
        initialState2 = InterpreterState env2 0 readC2 sendC2 chan2 chan1

    C.void $ Conc.forkIO $ S.evalStateT interpret' initialState1
    C.void $ Conc.forkIO $ S.evalStateT interpret' initialState2

    nWritten <- STM.atomically $ do
        a <- STM.readTVar readC1
        b <- STM.readTVar sendC1
        c <- STM.readTVar readC2
        d <- STM.readTVar sendC2

        if a == d && b == c
            then return d
            else STM.retry

    print nWritten
  where
    interpret' = C.forever $ runCurrent program

runCurrent :: V.Vector Stm -> Interpreter ()
runCurrent program = do
    pc <- S.gets intProgramCounter
    S.modify $ \s -> s { intProgramCounter = pc + 1 }

    runStm $ program V.! pc

runStm :: Stm -> Interpreter ()
runStm (Send value) = do
    v <- getValue value
    sender <- S.gets intSend
    sendCounter <- S.gets intSendCount

    S.liftIO $ STM.atomically $ do
        STM.modifyTVar sendCounter (+ 1)
        STM.writeTChan sender v
runStm (Set register value) = do
    env <- S.gets intEnv
    v <- getValue value

    S.modify $ \s -> s { intEnv = Map.insert register v env }
runStm (Add register value) = do
    env <- S.gets intEnv
    v <- getValue value

    S.modify $ \s -> s { intEnv = Map.insertWith (+) register v env }
runStm (Mul register value) = do
    env <- S.gets intEnv
    v <- getValue value

    let val = Map.findWithDefault 0 register env * v

    S.modify $ \s -> s { intEnv = Map.insert register val env }
runStm (Mod register value) = do
    env <- S.gets intEnv
    v <- getValue value

    let val = Map.findWithDefault 0 register env `mod` v

    S.modify $ \s -> s { intEnv = Map.insert register val env }
runStm (Receive register) = do
    receiver <- S.gets intReceive
    readCount <- S.gets intReadCount
    env <- S.gets intEnv

    v <- S.liftIO $ STM.atomically $ do
        STM.modifyTVar readCount (+ 1)
        STM.readTChan receiver

    S.modify $ \s -> s { intEnv = Map.insert register v env }
runStm (Jump value1 value2) = do
    pc <- S.gets intProgramCounter
    jumpLen <- (\x -> x - 1) <$> getValue value2
    shouldJump <- ifValue value1

    C.when shouldJump $ S.modify $ \s -> s
        { intProgramCounter = pc + fromIntegral jumpLen }

getValue :: Value -> Interpreter Integer
getValue (Literal n) = return n
getValue (RegisterValue register) = do
    env <- S.gets intEnv
    return $ Map.findWithDefault 0 register env

ifValue :: Value -> Interpreter Bool
ifValue (Literal n) = return $ n > 0
ifValue register = (> 0) <$> getValue register

{- Declare and parse statements. -}
type Register = Char

data Value = RegisterValue Register | Literal Integer deriving (Show)

data Stm
    = Send Value
    | Set Register Value
    | Add Register Value
    | Mul Register Value
    | Mod Register Value
    | Receive Register
    | Jump Value Value
  deriving (Show)

parseInput :: String -> Either P.ParseError [Stm]
parseInput = P.parse (statements <* P.eof) ""

statements :: P.Parsec String () [Stm]
statements = statement `P.endBy` P.char '\n'

statement :: P.Parsec String () Stm
statement = P.choice $ map P.try
    [ sendP
    , setP
    , addP
    , mulP
    , modP
    , receiveP
    , jumpP
    ]

sendP :: P.Parsec String () Stm
sendP = Send <$> (P.string "snd " *> valueP)

setP :: P.Parsec String () Stm
setP = Set <$> (P.string "set " *> registerP <* P.char ' ') <*> valueP

addP :: P.Parsec String () Stm
addP = Add <$> (P.string "add " *> registerP <* P.char ' ') <*> valueP

mulP :: P.Parsec String () Stm
mulP = Mul <$> (P.string "mul " *> registerP <* P.char ' ') <*> valueP

modP :: P.Parsec String () Stm
modP = Mod <$> (P.string "mod " *> registerP <* P.char ' ') <*> valueP

receiveP :: P.Parsec String () Stm
receiveP = Receive <$> (P.string "rcv " *> registerP)

jumpP :: P.Parsec String () Stm
jumpP = Jump <$> (P.string "jgz " *> valueP <* P.char ' ') <*> valueP

registerP :: P.Parsec String () Register
registerP = P.oneOf ['a'..'z']

valueP :: P.Parsec String () Value
valueP = P.choice [literal, registerValue]
  where
    literal = Literal <$> P.int
    registerValue = RegisterValue <$> registerP
