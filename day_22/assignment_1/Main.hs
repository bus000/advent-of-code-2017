{- Diagnostics indicate that the local grid computing cluster has been
 - contaminated with the Sporifica Virus. The grid computing cluster is a
 - seemingly-infinite two-dimensional grid of compute nodes. Each node is either
 - clean or infected by the virus.
 -
 - To prevent overloading the nodes (which would render them useless to the
 - virus) or detection by system administrators, exactly one virus carrier moves
 - through the network, infecting or cleaning nodes as it moves. The virus
 - carrier is always located on a single node in the network (the current node)
 - and keeps track of the direction it is facing.
 -
 - To avoid detection, the virus carrier works in bursts; in each burst, it
 - wakes up, does some work, and goes back to sleep. The following steps are all
 - executed in order one time each burst:
 -
 -  * If the current node is infected, it turns to its right. Otherwise, it
 -    turns to its left. (Turning is done in-place; the current node does not
 -    change.)
 -  * If the current node is clean, it becomes infected. Otherwise, it becomes
 -    cleaned. (This is done after the node is considered for the purposes of
 -    changing direction.)
 -  * The virus carrier moves forward one node in the direction it is facing.
 -
 - Diagnostics have also provided a map of the node infection status (your
 - puzzle input). Clean nodes are shown as .; infected nodes are shown as #.
 - This map only shows the center of the grid; there are many more nodes beyond
 - those shown, but none of them are currently infected.
 -
 - The virus carrier begins in the middle of the map facing up.
 -
 - For example, suppose you are given a map like this:
 -
 - ..#
 - #..
 - ...
 -
 - Then, the middle of the infinite grid looks like this, with the virus
 - carrier's position marked with [ ]:
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
 - The virus carrier is on a clean node, so it turns left, infects the node, and
 - moves left:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . # . . .
 - . . .[#]# . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - The virus carrier is on an infected node, so it turns right, cleans the node,
 - and moves up:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . .[.]. # . . .
 - . . . . # . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - Four times in a row, the virus carrier finds a clean, infects it, turns left,
 - and moves forward, ending in the same place and still facing up:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . #[#]. # . . .
 - . . # # # . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - Now on the same node as before, it sees an infection, which causes it to turn
 - right, clean the node, and move forward:
 -
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . # .[.]# . . .
 - . . # # # . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - After the above actions, a total of 7 bursts of activity had taken place. Of
 - them, 5 bursts of activity caused an infection.
 -
 - After a total of 70, the grid looks like this, with the virus carrier facing
 - up:
 -
 - . . . . . # # . .
 - . . . . # . . # .
 - . . . # . . . . #
 - . . # . #[.]. . #
 - . . # . # . . # .
 - . . . . . # # . .
 - . . . . . . . . .
 - . . . . . . . . .
 -
 - By this time, 41 bursts of activity caused an infection (though most of those
 - nodes have since been cleaned).
 -
 - After a total of 10000 bursts of activity, 5587 bursts will have caused an
 - infection.
 -
 - Given your actual map, after 10000 bursts of activity, how many bursts cause
 - a node to become infected? (Do not count nodes that begin infected.) -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.RWS as C
import qualified Data.Char as Char
import qualified Data.Word as Word
import Prelude ()

type Position = (Integer, Integer)
type Infected = Set Position
data Direction = U | D | L | R

main :: IO ()
main = do
    input <- parseInput <$> getContents

    print . C.getSum . snd =<< C.evalRWST (bursts input 10000) () ((0, 0), U)

bursts :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Infected
    -- ^ Original infected nodes.
    -> Word.Word
    -- ^ Number of bursts to run.
    -> m Infected
bursts infected n = C.foldM (\inf _ -> burst inf) infected [1..n]

{- | Perform single burst and return new state. When a node is infected the
 - writer sum is increased by 1. -}
burst :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Infected
    -- ^ Original infected nodes.
    -> m Infected
burst infected = turn *> newInfected <* move
  where
    newInfected = C.gets fst >>= \position ->
        if position `member` infected
            then return (deleteSet position infected)
            else C.tell 1 >> return (insertSet position infected)

    turn = C.modify $ \case
        (position, U) -> (position, bool L R (position `member` infected))
        (position, R) -> (position, bool U D (position `member` infected))
        (position, D) -> (position, bool R L (position `member` infected))
        (position, L) -> (position, bool D U (position `member` infected))

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
    idx = [(y, x) | x <- [size,size-1..(-size)], y <- [-size..size]]
    isHash x = x == '#'
