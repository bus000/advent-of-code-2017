module Main (main) where

import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right scanners -> print $ length . takeWhile (isHit scanners) $ [0..]

isHit :: [(Int, Int)] -> Int -> Bool
isHit scanners start = any hit $ zip [start..] [0..end]
  where
    end = maximum $ map fst scanners
    hit (n, pos) = case lookup pos scanners of
        Just scanL -> n `mod` (2 * scanL - 2) == 0
        Nothing -> False

parseInput :: String -> Either P.ParseError [(Int, Int)]
parseInput = P.parse (P.many line <* P.eof) ""

line :: P.Parsec String () (Int, Int)
line = (,) <$> P.int <* P.string ": " <*> P.int <* P.char '\n'
