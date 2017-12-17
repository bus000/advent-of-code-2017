module Main (main) where

import Prelude hiding (Either(..))

type SpiralCoordinate = Int
type EuclideanCoordinate = (Int, Int)

data Direction = Up | Down | Left | Right

main :: IO ()
main = do
    n <- read . init <$> getContents

    let (x, y) = toEuclidean n

    print $ abs x + abs y

toEuclidean :: SpiralCoordinate -> EuclideanCoordinate
toEuclidean n = foldr move (0, 0) (take (n - 1) movements)
  where
    movements = concat $ zipWith replicate lengths directions
    lengths = concatMap (replicate 2) [1..]
    directions = cycle [Right, Up, Left, Down]

    move Right (x, y) = (x + 1, y)
    move Up (x, y) = (x, y + 1)
    move Left (x, y) = (x - 1, y)
    move Down (x, y) = (x, y - 1)
