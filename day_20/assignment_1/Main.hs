module Main (main) where

import qualified System.Exit as Sys
import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right particles -> print $ head . L.sortOn fst $ zip particles [0..]

type Position = V.Vector Int
type Velocity = V.Vector Int
type Acceleration = V.Vector Int

data Particle = Particle Position Velocity Acceleration
  deriving (Eq, Show)

instance Ord Particle where
    Particle p1 v1 a1 `compare` Particle p2 v2 a2 =
        (a1', v1', p1') `compare` (a2', v2', p2')
      where
        p1' = V.sum . V.map abs $ p1
        v1' = V.sum . V.map abs $ v1
        a1' = V.sum . V.map abs $ a1
        p2' = V.sum . V.map abs $ p2
        v2' = V.sum . V.map abs $ v2
        a2' = V.sum . V.map abs $ a2

{- Input parsing. -}

parseInput :: String -> Either P.ParseError [Particle]
parseInput = P.parse (particleP `P.endBy` P.char '\n' <* P.eof) ""

particleP :: P.Parsec String () Particle
particleP = Particle <$>
    (P.string "p=" *> vector) <*>
    (P.string ", v=" *> vector) <*>
    (P.string ", a=" *> vector)
  where
    vector = V.fromList <$> P.between (P.char '<') (P.char '>') numbers
    numbers = P.int `P.sepBy` P.char ','
