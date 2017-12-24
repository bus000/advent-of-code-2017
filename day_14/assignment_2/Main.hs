module Main (main) where

import qualified KnotHash as KH
import qualified Data.Array as A
import qualified Graph as G

main :: IO ()
main = do
    input <- init <$> getContents

    let strings = map (\x -> input ++ "-" ++ show x) ([0..127] :: [Int])
        hash = concatMap KH.knothash strings
        bits = concatMap hexVal hash
        arr = A.listArray ((0,0), (127, 127)) bits

    print $ length . G.connectedComponentRoots . toGraph $ arr

toGraph :: A.Array (Int, Int) Bool -> G.Graph (Int, Int) ()
toGraph arr = G.fromListEmpty $ map (\x -> (x, trueNeighbours x)) trueIndices
  where
    trueIndices = filter (\x -> arr A.! x) allIndices
    allIndices = (,) <$> [x_start..x_end] <*> [y_start..y_end]
    ((x_start, y_start), (x_end, y_end)) = A.bounds arr

    trueNeighbours index = filter (`elem` trueIndices) (neighbours index)
    neighbours (x, y) = filter inside [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    inside (x, y) = x >= x_start && x <= x_end && y >= y_start && y <= y_end

hexVal :: Char -> [Bool]
hexVal '0' = [False, False, False, False]
hexVal '1' = [False, False, False, True ]
hexVal '2' = [False, False, True , False]
hexVal '3' = [False, False, True , True ]
hexVal '4' = [False, True , False, False]
hexVal '5' = [False, True , False, True ]
hexVal '6' = [False, True , True , False]
hexVal '7' = [False, True , True , True ]
hexVal '8' = [True , False, False, False]
hexVal '9' = [True , False, False, True ]
hexVal 'a' = [True , False, True , False]
hexVal 'b' = [True , False, True , True ]
hexVal 'c' = [True , True , False, False]
hexVal 'd' = [True , True , False, True ]
hexVal 'e' = [True , True , True , False]
hexVal 'f' = [True , True , True , True ]
hexVal _ = error "Not hex"
