module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Control.Monad.State as S
import qualified Control.Monad.Loops as C
import Data.Maybe (isNothing)

type Position = Int
type Jump = Int

data CPU = CPU (Map.Map Position Jump) Position deriving Show

main :: IO ()
main = do
    ns <- map read . lines <$> getContents

    let cpu = CPU (Map.fromList $ zip [0..] ns) 0

    print $ moveAll cpu

moveAll :: CPU -> Int
moveAll initState =
    length $ S.evalState (C.untilM move (isNothing <$> getVal)) initState

move :: S.State CPU ()
move = do
    (CPU jumps pos) <- S.get
    let jump = jumps ! pos
    S.put $ CPU (Map.insert pos (jump + 1) jumps) (pos + jump)

getVal :: S.State CPU (Maybe Int)
getVal = do
    (CPU jumps pos) <- S.get
    return $ Map.lookup pos jumps
