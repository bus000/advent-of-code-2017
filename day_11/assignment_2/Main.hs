{- How many steps away is the furthest he ever got from his starting
 - position? -}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List.Split as L

data Direction = N | NE | SE | S | SW | NW deriving (Show, Read)

type Position = (Int, Int)

main :: IO ()
main = do
    input <- map read . L.splitOn "," . map Char.toUpper <$> getContents

    let positions = allPos (0, 0) input
        distances = map (distance (0, 0)) positions

    print $ maximum distances

allPos :: Position -> [Direction] -> [Position]
allPos = scanl move
  where
    move (x, y) N = (x + 1, y + 0)
    move (x, y) NE = (x + 1, y - 1)
    move (x, y) SE = (x + 0, y - 1)
    move (x, y) S = (x - 1, y + 0)
    move (x, y) SW = (x - 1, y + 1)
    move (x, y) NW = (x + 0, y + 1)

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = maximum $ map abs [x2 - x1, y2 - y1, z2 - z1]
  where
    z1 = -(x1 + y1)
    z2 = -(x2 + y2)
