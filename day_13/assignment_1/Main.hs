module Main (main) where

import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right scanners -> print $ computePrice scanners

computePrice :: [(Int, Int)] -> Int
computePrice scanners = sum $ map price [0..end]
  where
    end = maximum $ map fst scanners
    price n = case lookup n scanners of
        Just scanL -> if hit scanL n then scanL * n else 0
        Nothing -> 0
    hit scanL n = n `mod` (2 * scanL - 2) == 0

parseInput :: String -> Either P.ParseError [(Int, Int)]
parseInput = P.parse (P.many line <* P.eof) ""

line :: P.Parsec String () (Int, Int)
line = (,) <$> P.int <* P.string ": " <*> P.int <* P.char '\n'
