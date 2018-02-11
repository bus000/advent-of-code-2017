{- A debugger program here is having an issue: it is trying to repair a memory
 - reallocation routine, but it keeps getting stuck in an infinite loop.
 -
 - In this area, there are sixteen memory banks; each memory bank can hold any
 - number of blocks. The goal of the reallocation routine is to balance the
 - blocks between the memory banks.
 -
 - The reallocation routine operates in cycles. In each cycle, it finds the
 - memory bank with the most blocks (ties won by the lowest-numbered memory
 - bank) and redistributes those blocks among the banks. To do this, it removes
 - all of the blocks from the selected bank, then moves to the next (by index)
 - memory bank and inserts one of the blocks. It continues doing this until it
 - runs out of blocks; if it reaches the last memory bank, it wraps around to
 - the first one.
 -
 - The debugger would like to know how many redistributions can be done before a
 - blocks-in-banks configuration is produced that has been seen before.
 -
 - For example, imagine a scenario with only four memory banks:
 -
 - * The banks start with 0, 2, 7, and 0 blocks. The third bank has the most
 -   blocks, so it is chosen for redistribution.
 - * Starting with the next bank (the fourth bank) and then continuing to the
 -   first bank, the second bank, and so on, the 7 blocks are spread out over
 -   the memory banks. The fourth, first, and second banks get two blocks each,
 -   and the third bank gets one back. The final result looks like this: 2 4 1
 -   2.
 - * Next, the second bank is chosen because it contains the most blocks (four).
 -   Because there are four memory banks, each gets one block. The result is: 3
 -   1 2 3.
 - * Now, there is a tie between the first and fourth memory banks, both of
 -   which have three blocks. The first bank wins the tie, and its three blocks
 -   are distributed evenly over the other three banks, leaving it with none: 0
 -   2 3 4.
 - * The fourth bank is chosen, and its four blocks are distributed such that
 -   each of the four banks receives one: 1 3 4 1.
 - * The third bank is chosen, and the same thing happens: 2 4 1 2.
 -
 - At this point, we've reached a state we've seen before: 2 4 1 2 was already
 - seen. The infinite loop is detected after the fifth block redistribution
 - cycle, and so the answer in this example is 5.
 -
 - Given the initial block counts in your puzzle input, how many redistribution
 - cycles must be completed before a configuration is produced that has been
 - seen before? -}
module Main (main) where

import Control.Arrow ((&&&))
import qualified Control.Monad.Loops as C
import qualified Control.Monad.State as S
import Data.List (group, sort)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector ((!))

type Position = Int
type Amount = Int

type Memory = (V.Vector Amount)

data ComputerState = ComputerState Position Memory (Set.Set Memory)

main :: IO ()
main = do
    memory <- V.fromList . map read . words <$> getContents

    let initialState = ComputerState 0 memory Set.empty

    print $ stepsTillDuplicate initialState

stepsTillDuplicate :: ComputerState -> Int
stepsTillDuplicate initState = Set.size steps
  where
    ComputerState _ _ steps = S.execState distributeTillDuplicate initState

distributeTillDuplicate :: S.State ComputerState ()
distributeTillDuplicate = (saveState >> distributeMax) `C.untilM_` seenbefore
  where
    seenbefore = do
        ComputerState _ mem prevmem <- S.get
        return $ mem `Set.member` prevmem
    saveState = S.modify $ \(ComputerState pos mem prevmem) ->
        ComputerState pos mem (Set.insert mem prevmem)

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
