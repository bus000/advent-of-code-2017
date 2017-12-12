module Main (main) where

import Data.List (group)

main :: IO ()
main = do
    (first:rest) <- init <$> getContents

    print $ sum (toSum $ [first] ++ rest ++ [first])

toSum :: String -> [Int]
toSum = map read . map (:[]) . concatMap (drop 1) . group
