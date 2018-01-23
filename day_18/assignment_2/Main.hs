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
