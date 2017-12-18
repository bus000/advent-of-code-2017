module Main (main) where

import qualified Control.Monad as C
import qualified Control.Monad.State as S
import qualified Data.List.Split as L
import qualified Data.Vector as V

type Position = Int
type ShiftWidth = Int

data KnotHashState = KnotHashState (V.Vector Int) [Position] ShiftWidth

main :: IO ()
main = do
    input <- map read . L.splitOn "," . init <$> getContents

    let KnotHashState endvec _ _ = knotHash 256 input

    print $ product (V.take 2 endvec)

knotHash :: Int -> [Int] -> KnotHashState
knotHash size reverses = S.execState (C.mapM reverseNext reverses) initialState
  where
    vec = V.fromList [0..size-1]
    positions = cycle [0..size-1]
    initialState = KnotHashState vec positions 0

type KnotHash a = S.State KnotHashState a

reverseNext :: Int -> KnotHash ()
reverseNext n = do
    indices <- moveIndices n

    let reverses = zip indices (reverse indices)
        uniqReverses = take (length reverses `div` 2) reverses

    C.mapM_ doReverse uniqReverses

    moveShift

moveIndices :: Int -> KnotHash [Int]
moveIndices n = do
    KnotHashState vec pos width <- S.get
    let pos' = drop n pos
    S.put (KnotHashState vec pos' width)

    return $ take n pos

moveShift :: KnotHash ()
moveShift = do
    KnotHashState vec pos width <- S.get

    let pos' = drop width pos
        width' = width + 1

    S.put $ KnotHashState vec pos' width'

doReverse :: (Int, Int) -> KnotHash ()
doReverse (i1, i2) = do
    KnotHashState vec pos width <- S.get

    let val1 = vec V.! i1
        val2 = vec V.! i2
        vec' = vec V.// [(i1, val2), (i2, val1)]

    S.put (KnotHashState vec' pos width)
