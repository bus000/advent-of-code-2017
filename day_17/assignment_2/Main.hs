module Main (main) where

import qualified Data.List as L

main :: IO ()
main = do
    input <- read <$> getContents

    let allMoves = take 50000000 $ positions input
        last1 = last . L.elemIndices 1 $ allMoves

    print last1

positions :: Int -> [Int]
positions shift = scanl (nextPosition shift) 0 [1..]

nextPosition :: Int -> Int -> Int -> Int
nextPosition shift pos len
    | moves >= len - pos = moves - (len - pos) + 1
    | otherwise = pos + moves + 1
  where
    moves = shift `mod` len
