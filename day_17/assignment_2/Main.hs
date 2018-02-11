{- The spinlock does not short-circuit. Instead, it gets more angry. At least,
 - you assume that's what happened; it's spinning significantly faster than it
 - was a moment ago.
 -
 - You have good news and bad news.
 -
 - The good news is that you have improved calculations for how to stop the
 - spinlock. They indicate that you actually need to identify the value after 0
 - in the current state of the circular buffer.
 -
 - The bad news is that while you were determining this, the spinlock has just
 - finished inserting its fifty millionth value (50000000).
 -
 - What is the value after 0 the moment 50000000 is inserted? -}
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
