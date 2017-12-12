module Main (main) where

import qualified Control.Monad.State as S
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import Control.Arrow ((&&&))
import Data.List (group, sort)
import qualified Control.Monad.Loops as C

type Position = Int
type Amount = Int

type Memory = (V.Vector Amount)

data ComputerState = ComputerState Position Memory (Set.Set Memory)

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
findMax = S.modify $ \(ComputerState pos mem prevmem) ->
    ComputerState (V.maxIndex mem) mem prevmem

followingIndices :: Memory -> Position -> [Position]
followingIndices mem n = drop (n+1) (cycle [0..V.length mem - 1])
