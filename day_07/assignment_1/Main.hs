module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as L

main :: IO ()
main = print =<< getRoots <$> getContents

{- | A root is any alphanumeric word occuring only once since all other will
 - be both in their own line and in some child list. -}
getRoots :: String -> [String]
getRoots = head . filter length1 . L.group . L.sort . words . filter normalchar
  where
    normalchar x = Char.isAlpha x || Char.isSpace x
    length1 x = length x == 1
