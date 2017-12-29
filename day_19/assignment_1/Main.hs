module Main (main) where

import qualified Control.Monad as C
import qualified Control.Monad.RWS.Lazy as RWS
import Prelude hiding (Either(..))
import qualified Data.Array as A
import qualified Data.Char as Char

main :: IO ()
main = do
    input <- getContents

    let lineLen = length . head . lines $ input
        lineN = length . lines $ input
        rawInput = filter (/= '\n') input
        tubeMap = A.listArray ((0, 0), (lineN - 1, lineLen - 1)) rawInput
        startPos = findStartPosition tubeMap

    putStrLn $ snd $ RWS.evalRWS move tubeMap (startPos, Down, True)

type Position = (Int, Int)
type Turning = Bool
data Direction = Up | Down | Left | Right deriving (Show, Eq, Enum)
type Tube a = RWS.RWS (A.Array Position Char) [Char] (Position, Direction, Turning) a
data Action = Turn | Continue | Abort | ReportContinue Char

move :: Tube ()
move = do
    tubeMap <- RWS.ask
    (position, direction, turning) <- RWS.get

    case nextAction tubeMap position direction turning of
        Abort -> return ()
        Turn -> turn
        Continue -> forward >> move
        ReportContinue c -> RWS.tell [c] >> forward >> move
  where
    nextAction tubeMap pos dir False = case tubeMap A.! pos of
        c | c == '|' || c == '-' -> Continue
        c | Char.isAlpha c -> ReportContinue c
        c | c == '+' -> Turn
        _ -> Abort

    nextAction tubeMap pos dir True
        | dir == Up || dir == Down = case tubeMap A.! pos of
            c | c == '|' -> Continue
            c | Char.isAlpha c -> ReportContinue c
            _ -> Abort
        | dir == Left || dir == Right = case tubeMap A.! pos of
            c | c == '-' -> Continue
            c | Char.isAlpha c -> ReportContinue c
            _ -> Abort

turn :: Tube ()
turn = do
    (position, direction, _) <- RWS.get

    let directions' = orthogonal direction
        positions' = map (movePos position) directions'
        posdirs = zip positions' directions'

    C.mapM_ (\(pos, dir) -> RWS.put (pos, dir, True) >> move) posdirs
  where
    movePos (x, y) Up    = (x - 1, y)
    movePos (x, y) Down  = (x + 1, y)
    movePos (x, y) Left  = (x, y - 1)
    movePos (x, y) Right = (x, y + 1)

    orthogonal Up = [Left, Right]
    orthogonal Down = [Left, Right]
    orthogonal Left = [Up, Down]
    orthogonal Right = [Up, Down]

forward :: Tube ()
forward = do
    (position, direction, _) <- RWS.get
    let position' = movePos position direction
    RWS.put (position', direction, False)
  where
    movePos (x, y) Up    = (x - 1, y)
    movePos (x, y) Down  = (x + 1, y)
    movePos (x, y) Left  = (x, y - 1)
    movePos (x, y) Right = (x, y + 1)

findStartPosition :: A.Array Position Char -> Position
findStartPosition = fst . head . filter (not . Char.isSpace . snd) . A.assocs
