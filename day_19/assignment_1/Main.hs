{- Somehow, a network packet got lost and ended up here. It's trying to follow a
 - routing diagram (your puzzle input), but it's confused about where to go.
 -
 - Its starting point is just off the top of the diagram. Lines (drawn with |,
 - -, and +) show the path it needs to take, starting by going down onto the
 - only line connected to the top of the diagram. It needs to follow this path
 - until it reaches the end (located somewhere within the diagram) and stop
 - there.
 -
 - Sometimes, the lines cross over each other; in these cases, it needs to
 - continue going the same direction, and only turn left or right when there's
 - no other option. In addition, someone has left letters on the line; these
 - also don't change its direction, but it can use them to keep track of where
 - it's been. For example:
 -
 -      |
 -      |  +--+
 -      A  |  C
 -  F---|----E|--+
 -      |  |  |  D
 -      +B-+  +--+
 -
 - Given this diagram, the packet needs to take the following path:
 -
 -  * Starting at the only line touching the top of the diagram, it must go
 -    down, pass through A, and continue onward to the first +.
 -  * Travel right, up, and right, passing through B in the process.
 -  * Continue down (collecting C), right, and up (collecting D).
 -  * Finally, go all the way left through E and stopping at F.
 -
 - Following the path to the end, the letters it sees on its path are ABCDEF.
 -
 - The little packet looks up at you, hoping you can help it find the way. What
 - letters will it see (in the order it would see them) if it follows the path?
 - (The routing diagram is very wide; make sure you view it without line
 - wrapping.) -}
module Main (main) where

import qualified Control.Monad as C
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Data.Array as A
import qualified Data.Char as Char
import Prelude hiding (Either(..))

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
type TubeMap = A.Array Position Char
type Turning = Bool
data Direction = Up | Down | Left | Right deriving (Show, Eq, Enum)
type Tube a = RWS.RWS TubeMap [Char] (Position, Direction, Turning) a
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
    nextAction tubeMap pos _ False = case tubeMap A.! pos of
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

    nextAction _ _ _ _ = Abort

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
