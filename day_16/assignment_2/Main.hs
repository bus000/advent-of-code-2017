module Main (main) where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Debug.Trace as Debug
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

data Move a = Spin Int | Exchange Int Int | Partner a a deriving (Show)

type Lineup a = V.Vector a

main :: IO ()
main = do
    input <- parseInput . init <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right moves -> print $ dances initLine moves 1000000000
  where
    initLine = V.fromList ['a'..'p']

dances :: (Eq a, Ord a) => Lineup a -> [Move a] -> Int -> Lineup a
dances line moves n = iterate (`dance` moves) dup !! (n - bb)
  where
    ab = (n `div` (id2 - id1)) * (id2 - id1) + id1
    bb = head . dropWhile (> n) $ [ab,ab-(id2 - id1)..]
    [id1, id2] = take 2 . L.elemIndices dup $ positions
    dup = fst . Map.findMax . head . dropWhile onlyUnique .
        scanl count Map.empty $ positions
    positions = iterate (`dance` moves) line
    onlyUnique m
        | Map.null m = True
        | otherwise = case Map.findMax m of
            (_, v) -> v < 2
    count m l = Map.insertWith (+) l 1 m

dance :: Eq a => Lineup a -> [Move a] -> Lineup a
dance = foldl move
  where
    move line (Spin n) = spin line n
    move line (Exchange n1 n2) = exchange line n1 n2
    move line (Partner a1 a2) = partner line a1 a2

spin :: Lineup a -> Int -> Lineup a
spin line n = V.drop (len-n) line V.++ V.take (len-n) line
  where
    len = V.length line

exchange :: Lineup a -> Int -> Int -> Lineup a
exchange line i1 i2 = line V.// [(i1, v2), (i2, v1)]
  where
    v1 = line V.! i1
    v2 = line V.! i2

partner :: Eq a => Lineup a -> a -> a -> Lineup a
partner line v1 v2 = line V.// [(i1, v2), (i2, v1)]
  where
    Just i1 = V.elemIndex v1 line
    Just i2 = V.elemIndex v2 line

parseInput :: String -> Either P.ParseError [Move Char]
parseInput = P.parse (pMoves <* P.eof) ""

pMoves :: P.Parsec String () [Move Char]
pMoves = pMove `P.sepBy` P.char ','

pMove :: P.Parsec String () (Move Char)
pMove = P.choice [pSpin, pExchange, pPartner]
  where
    pSpin = Spin <$> (P.char 's' *> P.int)
    pExchange = Exchange <$> (P.char 'x' *> P.int <* P.char '/') <*> P.int
    pPartner = Partner <$> (P.char 'p' *> P.anyChar <* P.char '/') <*> P.anyChar
