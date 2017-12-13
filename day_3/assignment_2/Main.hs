module Main (main) where

import Prelude hiding (Either(..))

type EuclideanCoordinate = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Show)

type SpiralSpace = [Int]

main :: IO ()
main = do
    n <- read . init <$> getContents

    print $ head . dropWhile (< n) $ spiralSpace

spiralSpace :: SpiralSpace
spiralSpace = map fst space
  where
    space = scanl genSpace (1, initialSpace) (followMovement (0, 0) movements)

    genSpace (_, prevspace) pos =
        let current = sum (map prevspace (surrounding pos))
        in (current, \x -> if x == pos then current else prevspace x)

    initialSpace = (\x -> if x == (0, 0) then 1 else 0)

    surrounding (x, y) =
        [ (x-1, y+1), (x, y+1), (x+1, y+1)
        , (x-1, y  ),           (x+1, y  )
        , (x-1, y-1), (x, y-1), (x+1, y-1)
        ]

movements :: [Direction]
movements = concat $ zipWith replicate lengths directions
  where
    lengths = concatMap (replicate 2) [1..]
    directions = cycle [Right, Up, Left, Down]

followMovement :: EuclideanCoordinate -> [Direction] -> [EuclideanCoordinate]
followMovement initpos = drop 1 . scanl move initpos
  where
    move (x, y) Right = (x + 1, y)
    move (x, y) Up = (x, y + 1)
    move (x, y) Left = (x - 1, y)
    move (x, y) Down = (x, y - 1)
