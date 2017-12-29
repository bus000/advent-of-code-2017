module Main (main) where

import qualified Control.Monad as C
import qualified Control.Monad.RWS.Lazy as RWS
import Prelude hiding (Either(..))
import qualified Data.Array as A
import qualified Data.Char as Char
import qualified Debug.Trace as Debug

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

move :: Tube ()
move = do
    tubeMap <- RWS.ask
    (position, direction, turning) <- RWS.get

    let position' = movePos position direction

    case (direction, turning, tubeMap A.! position') of
        (_, False, c)
            | c == '|' || c == '-' -> continue position' direction
            | Char.isAlpha c -> RWS.tell [c] >> continue position' direction
            | c == '+' -> turn position' direction

        (Up, True, '|') -> continue position' direction
        (Down, True, '|') -> continue position' direction
        (Left, True, '-') -> continue position' direction
        (Right, True, '-') -> continue position' direction
        (_, True, c)
            | Char.isAlpha c -> RWS.tell [c] >> continue position' direction

        _ -> return ()
  where
    movePos (x, y) Up    = (x - 1, y)
    movePos (x, y) Down  = (x + 1, y)
    movePos (x, y) Left  = (x, y - 1)
    movePos (x, y) Right = (x, y + 1)

    continue newPosition direction = do
        RWS.put (newPosition, direction, False)
        move

turn :: Position -> Direction -> Tube ()
turn newPosition curDir =
    C.mapM_ (\dir -> RWS.put (newPosition, dir, True) >> move) directions
  where
    directions = filter (`notElem` [curDir, opposite curDir]) allDirections
    allDirections = enumFrom . toEnum $ 0

    opposite Up = Down
    opposite Down = Up
    opposite Left = Right
    opposite Right = Left

findStartPosition :: A.Array Position Char -> Position
findStartPosition = fst . head . filter (not . Char.isSpace . snd) . A.assocs
