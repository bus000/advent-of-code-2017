{- Suddenly, the GPU contacts you, asking for help. Someone has asked it to
 - simulate too many particles, and it won't be able to finish them all in time
 - to render the next frame at this rate.
 -
 - It transmits to you a buffer (your puzzle input) listing each particle in
 - order (starting with particle 0, then particle 1, particle 2, and so on). For
 - each particle, it provides the X, Y, and Z coordinates for the particle's
 - position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.
 -
 - Each tick, all particles are updated simultaneously. A particle's properties
 - are updated in the following order:
 -
 -  * Increase the X velocity by the X acceleration.
 -  * Increase the Y velocity by the Y acceleration.
 -  * Increase the Z velocity by the Z acceleration.
 -  * Increase the X position by the X velocity.
 -  * Increase the Y position by the Y velocity.
 -  * Increase the Z position by the Z velocity.
 -
 - Because of seemingly tenuous rationale involving z-buffering, the GPU would
 - like to know which particle will stay closest to position <0,0,0> in the long
 - term.  Measure this using the Manhattan distance, which in this situation is
 - simply the sum of the absolute values of a particle's X, Y, and Z position.
 -
 - For example, suppose you are only given two particles, both of which stay
 - entirely on the X-axis (for simplicity). Drawing the current states of
 - particles 0 and 1 (in that order) with an adjacent a number line and diagram
 - of current X positions (marked in parenthesis), the following would take
 - place:
 -
 - p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
 - p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
 -
 - p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
 - p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
 -
 - p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
 - p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
 -
 - p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
 - p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   
 -
 - At this point, particle 1 will never be closer to <0,0,0> than particle 0,
 - and so, in the long run, particle 0 will stay closest.
 -
 - Which particle will stay closest to position <0,0,0> in the long term? -}
module Main (main) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right particles -> print $ smallestInd particles

smallestInd :: Ord a => [a] -> Int
smallestInd xs = snd . head . L.sortOn fst $ zip xs [0..]

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
