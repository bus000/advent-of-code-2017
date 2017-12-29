module Main (main) where

import qualified System.Exit as Sys
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right particles -> print $ length . simulate 1000 $ particles

simulate :: Int -> [Particle] -> [Particle]
simulate steps particles = iterate simulate' particles !! steps
  where
    simulate'
        = concat
        . filter ((==) 1 . length)
        . L.groupOn position
        . L.sortOn position
        . map move

type Position = V.Vector Int
type Velocity = V.Vector Int
type Acceleration = V.Vector Int

data Particle = Particle Position Velocity Acceleration
  deriving (Eq, Show)

position :: Particle -> Position
position (Particle p _ _) = p

move :: Particle -> Particle
move (Particle p v a) = Particle p' v' a
  where
    v' = V.zipWith (+) v a
    p' = V.zipWith (+) p v'

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
