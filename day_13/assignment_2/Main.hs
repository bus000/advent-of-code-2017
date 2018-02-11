{- Now, you need to pass through the firewall without being caught - easier said
 - than done.
 -
 - You can't control the speed of the packet, but you can delay it any number of
 - picoseconds. For each picosecond you delay the packet before beginning your
 - trip, all security scanners move one step. You're not in the firewall during
 - this time; you don't enter layer 0 until you stop delaying the packet.
 -
 - In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you
 - won't get caught:
 -
 - State after delaying:
 -  0   1   2   3   4   5   6
 - [ ] [S] ... ... [ ] ... [ ]
 - [ ] [ ]         [ ]     [ ]
 - [S]             [S]     [S]
 -                 [ ]     [ ]
 -
 - Picosecond 10:
 -  0   1   2   3   4   5   6
 - ( ) [S] ... ... [ ] ... [ ]
 - [ ] [ ]         [ ]     [ ]
 - [S]             [S]     [S]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - ( ) [ ] ... ... [ ] ... [ ]
 - [S] [S]         [S]     [S]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 - Picosecond 11:
 -  0   1   2   3   4   5   6
 - [ ] ( ) ... ... [ ] ... [ ]
 - [S] [S]         [S]     [S]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - [S] (S) ... ... [S] ... [S]
 - [ ] [ ]         [ ]     [ ]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 - Picosecond 12:
 -  0   1   2   3   4   5   6
 - [S] [S] (.) ... [S] ... [S]
 - [ ] [ ]         [ ]     [ ]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - [ ] [ ] (.) ... [ ] ... [ ]
 - [S] [S]         [S]     [S]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 - Picosecond 13:
 -  0   1   2   3   4   5   6
 - [ ] [ ] ... (.) [ ] ... [ ]
 - [S] [S]         [S]     [S]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - [ ] [S] ... (.) [ ] ... [ ]
 - [ ] [ ]         [ ]     [ ]
 - [S]             [S]     [S]
 -                 [ ]     [ ]
 -
 - Picosecond 14:
 -  0   1   2   3   4   5   6
 - [ ] [S] ... ... ( ) ... [ ]
 - [ ] [ ]         [ ]     [ ]
 - [S]             [S]     [S]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - [ ] [ ] ... ... ( ) ... [ ]
 - [S] [S]         [ ]     [ ]
 - [ ]             [ ]     [ ]
 -                 [S]     [S]
 -
 - Picosecond 15:
 -  0   1   2   3   4   5   6
 - [ ] [ ] ... ... [ ] (.) [ ]
 - [S] [S]         [ ]     [ ]
 - [ ]             [ ]     [ ]
 -                 [S]     [S]
 -
 -  0   1   2   3   4   5   6
 - [S] [S] ... ... [ ] (.) [ ]
 - [ ] [ ]         [ ]     [ ]
 - [ ]             [S]     [S]
 -                 [ ]     [ ]
 -
 - Picosecond 16:
 -  0   1   2   3   4   5   6
 - [S] [S] ... ... [ ] ... ( )
 - [ ] [ ]         [ ]     [ ]
 - [ ]             [S]     [S]
 -                 [ ]     [ ]
 -
 -  0   1   2   3   4   5   6
 - [ ] [ ] ... ... [ ] ... ( )
 - [S] [S]         [S]     [S]
 - [ ]             [ ]     [ ]
 -                 [ ]     [ ]
 -
 - Because all smaller delays would get you caught, the fewest number of
 - picoseconds you would need to delay to get through safely is 10.
 -
 - What is the fewest number of picoseconds that you need to delay the packet to
 - pass through the firewall without being caught? -}
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
