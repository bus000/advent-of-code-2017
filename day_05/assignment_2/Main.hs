{- Now, the jumps are even stranger: after each jump, if the offset was three or
 - more, instead decrease it by 1. Otherwise, increase it by 1 as before.
 -
 - Using this rule with the above example, the process now takes 10 steps, and
 - the offset values after finding the exit are left as 2 3 2 3 -1.
 -
 - How many steps does it now take to reach the exit? -}
module Main (main) where

import qualified Control.Monad.Loops as C
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
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
        newjump = if jump >= 3 then jump - 1 else jump + 1
    S.put $ CPU (Map.insert pos newjump jumps) (pos + jump)

getVal :: S.State CPU (Maybe Int)
getVal = do
    (CPU jumps pos) <- S.get
    return $ Map.lookup pos jumps
