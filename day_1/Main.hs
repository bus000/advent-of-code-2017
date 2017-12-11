module Main (main) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Sum of all digits matching the next.
-- List is circular.

main :: IO ()
main = do
    (first, rest) <- T.splitAt 1 . T.init <$> T.getContents

    let input = first <> rest <> first
        toSum :: [Int]
        toSum
            = map read
            . map (:[])
            . T.unpack
            . T.concat
            . map (T.drop 1)
            $ T.group input

    print $ sum toSum
