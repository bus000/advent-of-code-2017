module Main (main) where

import Data.List (group)
import Control.Arrow ((&&&))
import qualified Data.Char as C

main :: IO ()
main = do
    (input, l) <- (cycle &&& length) . map C.digitToInt . init <$> getContents

    let matched = take l (zip input (drop (l `div` 2) input))
        toSum = map fst . filter (uncurry (==)) $ matched

    print $ sum toSum
