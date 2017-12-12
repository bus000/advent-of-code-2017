module Main (main) where

import Data.List (group, sort)

main :: IO ()
main = do
    valid <- filter validpass . lines <$> getContents

    print $ length valid

validpass :: String -> Bool
validpass = not . any (> 1) . map length . group . sort . map sort . words
