module Main (main) where

import qualified KnotHash as KH

main :: IO ()
main = do
    input <- init <$> getContents

    let strings = map (\x -> input ++ "-" ++ show x) ([0..127] :: [Int])
        hash = concatMap KH.knothash strings
        bits = concatMap hexVal hash

    print $ length (filter id bits)

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
