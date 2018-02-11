{- As you go to remove the virus from the infected nodes, it evolves to resist
 - your attempt.
 -
 - Now, before it infects a clean node, it will weaken it to disable your
 - defenses.  If it encounters an infected node, it will instead flag the node
 - to be cleaned in the future. So:
 -
 -  * Clean nodes become weakened.
 -  * Weakened nodes become infected.
 -  * Infected nodes become flagged.
 -  * Flagged nodes become clean.
 -
 - Every node is always in exactly one of the above states.
 -
 - The virus carrier still functions in a similar way, but now uses the
 - following logic during its bursts of action:
 -
 -  * Decide which way to turn based on the current node:
 -    - If it is clean, it turns left.
 -    - If it is weakened, it does not turn, and will continue moving in the
 -      same direction.
 -    - If it is infected, it turns right.
 -    - If it is flagged, it reverses direction, and will go back the way it
 -      came.
 -  * Modify the state of the current node, as described above.
 -  * The virus carrier moves forward one node in the direction it is facing.
 -
 - Start with the same map (still using . for clean and # for infected) and
 - still with the virus carrier starting in the middle and facing up.
 -
 - Using the same initial state as the previous example, and drawing weakened as
 - W and flagged as F, the middle of the infinite grid looks like this, with the
 - virus carrier's position again marked with [ ]:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . # . . .
 - . . . #[.]. . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - This is the same as before, since no initial nodes are weakened or flagged.
 - The virus carrier is on a clean node, so it still turns left, instead weakens
 - the node, and moves left:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . # . . .
 - . . .[#]W . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - The virus carrier is on an infected node, so it still turns right, instead
 - flags the node, and moves up:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . .[.]. # . . .
 - . . . F W . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - This process repeats three more times, ending on the previously-flagged node
 - and facing right:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . W W . # . . .
 - . . W[F]W . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - Finding a flagged node, it reverses direction and cleans the node:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . W W . # . . .
 - . .[W]. W . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - The weakened node becomes infected, and it continues in the same direction:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . W W . # . . .
 - .[.]# . W . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - Of the first 100 bursts, 26 will result in infection. Unfortunately, another
 - feature of this evolved virus is speed; of the first 10000000 bursts, 2511944
 - will result in infection.
 -
 - Given your actual map, after 10000000 bursts of activity, how many bursts
 - cause a node to become infected? (Do not count nodes that begin infected.) -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad.RWS.Strict as C
import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Word as Word
import Prelude ()

type Position = (Integer, Integer)
type Infected = Set Position
type Weakened = Set Position
type Flagged = Set Position
data Cluster = Cluster !Infected !Weakened !Flagged
data Direction = U | D | L | R

main :: IO ()
main = do
    input <- parseInput <$> getContents

    let cluster = Cluster input Set.empty Set.empty

    print . C.getSum . snd =<<
        C.evalRWST (bursts cluster 10000000) () ((0, 0), U)

bursts :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Cluster
    -- ^ Original infected nodes.
    -> Word.Word
    -- ^ Number of bursts to run.
    -> m Cluster
bursts infected n = F.foldrM (\_ inf -> burst inf) infected [1..n]

{- | Perform single burst and return new state. When a node is infected the
 - writer sum is increased by 1. -}
burst :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Cluster
    -- ^ Original infected nodes.
    -> m Cluster
burst (Cluster infected weakened flagged) = turn *> newCluster <* move
  where
    newCluster = C.gets fst >>= \position -> case () of
        _ | position `member` infected ->
            return $ Cluster (deleteSet position infected) weakened
                (insertSet position flagged)
        _ | position `member` weakened -> do
            C.tell 1
            return $ Cluster (insertSet position infected)
                (deleteSet position weakened) flagged
        _ | position `member` flagged ->
            return $ Cluster infected weakened (deleteSet position flagged)
        _ -> return $ Cluster infected (insertSet position weakened) flagged

    turn = C.modify $ \case
        (position, U) -> case () of
            _ | position `member` infected -> (position, R)
            _ | position `member` weakened -> (position, U)
            _ | position `member` flagged -> (position, D)
            _ -> (position, L)

        (position, D) -> case () of
            _ | position `member` infected -> (position, L)
            _ | position `member` weakened -> (position, D)
            _ | position `member` flagged -> (position, U)
            _ -> (position, R)

        (position, L) -> case () of
            _ | position `member` infected -> (position, U)
            _ | position `member` weakened -> (position, L)
            _ | position `member` flagged -> (position, R)
            _ -> (position, D)

        (position, R) -> case () of
            _ | position `member` infected -> (position, D)
            _ | position `member` weakened -> (position, R)
            _ | position `member` flagged -> (position, L)
            _ -> (position, U)

    move = C.modify $ \case
        ((x, y), U) -> ((x    , y + 1), U)
        ((x, y), D) -> ((x    , y - 1), D)
        ((x, y), L) -> ((x - 1, y    ), L)
        ((x, y), R) -> ((x + 1, y    ), R)

parseInput :: LText -> Infected
parseInput txt = setFromList . map snd . filter (isHash . fst). zip nodes $ idx
  where
    nodes = filter (not . Char.isSpace) . toList $ txt
    size = flip div 2 . fromIntegral . length . lines $ txt
    idx = [(y, x) | x <- [size, size-1..(-size)], y <- [-size..size]]
    isHash x = x == '#'
