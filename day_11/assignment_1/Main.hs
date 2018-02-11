{- Crossing the bridge, you've barely reached the other side of the stream when
 - a program comes up to you, clearly in distress. "It's my child process," she
 - says, "he's gotten lost in an infinite grid!"
 -
 - Fortunately for her, you have plenty of experience with infinite grids.
 -
 - Unfortunately for you, it's a hex grid.
 -
 - The hexagons ("hexes") in this grid are aligned such that adjacent hexes can
 - be found to the north, northeast, southeast, south, southwest, and northwest:
 -
 -   \ n  /
 - nw +--+ ne
 -   /    \
 - -+      +-
 -   \    /
 - sw +--+ se
 -   / s  \
 -
 - You have the path the child process took. Starting where he started, you need
 - to determine the fewest number of steps required to reach him. (A "step"
 - means to move from the hex you are in to any adjacent hex.)
 -
 - For example:
 -
 -  * ne,ne,ne is 3 steps away.
 -  * ne,ne,sw,sw is 0 steps away (back where you started).
 -  * ne,ne,s,s is 2 steps away (se,se).
 -  * se,sw,se,sw,sw is 3 steps away (s,s,sw). -}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List.Split as L

data Direction = N | NE | SE | S | SW | NW deriving (Show, Read)

type Position = (Int, Int)

main :: IO ()
main = do
    input <- map read . L.splitOn "," . map Char.toUpper <$> getContents

    print $ distance (0, 0) (endPos (0, 0) input)

endPos :: Position -> [Direction] -> Position
endPos = foldr move
  where
    move N (x, y) = (x + 1, y)
    move NE (x, y) = (x, y + 1)
    move SE (x, y) = (x - 1, y + 1)
    move S (x, y) = (x - 1, y)
    move SW (x, y) = (x, y - 1)
    move NW (x, y) = (x + 1, y - 1)

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = maximum $ map abs [x2 - x1, y2 - y1, z2 - z1]
  where
    z1 = -(x1 + y1)
    z2 = -(x2 + y2)
