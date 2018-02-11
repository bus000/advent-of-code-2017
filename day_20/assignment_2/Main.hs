{- To simplify the problem further, the GPU would like to remove any particles
 - that collide. Particles collide if their positions ever exactly match.
 - Because particles are updated simultaneously, more than two particles can
 - collide at the same time and place. Once particles collide, they are removed
 - and cannot collide with anything else after that tick.
 -
 - For example:
 -
 - p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
 - p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
 - p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
 - p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
 -
 - p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
 - p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
 - p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
 - p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
 -
 - p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
 - p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
 - p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
 - p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
 -
 - ------destroyed by collision------
 - ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
 - ------destroyed by collision------                      (3)
 - p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
 -
 - In this example, particles 0, 1, and 2 are simultaneously destroyed at the
 - time and place marked X. On the next tick, particle 3 passes through
 - unharmed.
 -
 - How many particles are left after all collisions are resolved? -}
module Main (main) where

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Vector as V
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

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
