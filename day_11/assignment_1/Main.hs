module Main (main) where

import qualified Data.List.Split as L
import qualified Data.Char as Char

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
