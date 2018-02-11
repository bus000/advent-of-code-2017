{- Out of curiosity, the debugger would also like to know the size of the loop:
 - starting from a state that has already been seen, how many block
 - redistribution cycles must be performed before that same state is seen again?
 -
 - In the example above, 2 4 1 2 is seen again after four cycles, and so the
 - answer in that example would be 4.
 -
 - How many cycles are in the infinite loop that arises from the configuration
 - in your puzzle input? -}
module Main (main) where

import Control.Arrow ((&&&))
import qualified Control.Monad.Loops as C
import qualified Control.Monad.State as S
import Data.List (group, sort)
import qualified Data.Vector as V
import Data.Vector ((!))

type Position = Int
type Amount = Int

type Memory = (V.Vector Amount)

data ComputerState = ComputerState Position Memory [Memory]

main :: IO ()
main = do
    memory <- V.fromList . map read . words <$> getContents

    let initialState = ComputerState 0 memory []

    print $ cycleLength initialState

cycleLength :: ComputerState -> Int
cycleLength initState = (+) 1 . length . takeWhile (/= end) $ steps
  where
    ComputerState _ end steps = S.execState distributeTillDuplicate initState

distributeTillDuplicate :: S.State ComputerState ()
distributeTillDuplicate = (saveState >> distributeMax) `C.untilM_` seenbefore
  where
    seenbefore = do
        ComputerState _ mem prevmem <- S.get
        return $ mem `elem` prevmem
    saveState = S.modify $ \(ComputerState pos mem prevmem) ->
        ComputerState pos mem (mem:prevmem)

distributeMax :: S.State ComputerState ()
distributeMax = findMax >> distribute

distribute :: S.State ComputerState ()
distribute = S.modify $ \(ComputerState pos mem prevmem) ->
    let available = mem ! pos
        positions = take available $ followingIndices mem pos
        updates = (pos, -available):
            (map (head &&& length) . group . sort $ positions)
        addIndex i x = (+) x . sum . map snd . filter ((== i) . fst) $ updates
        newmem = V.imap addIndex mem
    in ComputerState (last positions) newmem prevmem

findMax :: S.State ComputerState ()
findMax = S.modify $ \(ComputerState _ mem prevmem) ->
    ComputerState (V.maxIndex mem) mem prevmem

followingIndices :: Memory -> Position -> [Position]
followingIndices mem n = drop (n+1) (cycle [0..V.length mem - 1])
