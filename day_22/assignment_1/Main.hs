{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.RWS as C
import qualified Data.Char as Char
import qualified Data.Word as Word
import Prelude ()

type Position = (Integer, Integer)
type Infected = Set Position
data Direction = U | D | L | R

main :: IO ()
main = do
    input <- parseInput <$> getContents

    print . C.getSum . snd =<< C.evalRWST (bursts input 10000) () ((0, 0), U)

bursts :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Infected
    -- ^ Original infected nodes.
    -> Word.Word
    -- ^ Number of bursts to run.
    -> m Infected
bursts infected n = C.foldM (\inf _ -> burst inf) infected [1..n]

{- | Perform single burst and return new state. When a node is infected the
 - writer sum is increased by 1. -}
burst :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Infected
    -- ^ Original infected nodes.
    -> m Infected
burst infected = turn *> newInfected <* move
  where
    newInfected = C.gets fst >>= \position ->
        if position `member` infected
            then return (deleteSet position infected)
            else C.tell 1 >> return (insertSet position infected)

    turn = C.modify $ \case
        (position, U) -> (position, bool L R (position `member` infected))
        (position, R) -> (position, bool U D (position `member` infected))
        (position, D) -> (position, bool R L (position `member` infected))
        (position, L) -> (position, bool D U (position `member` infected))

    move = C.modify $ \case
        ((x, y), U) -> ((x    , y + 1), U)
        ((x, y), D) -> ((x    , y - 1), D)
        ((x, y), L) -> ((x - 1, y    ), L)
        ((x, y), R) -> ((x + 1, y    ), R)

parseInput :: LText -> Infected
parseInput txt = setFromList . map snd . filter (isHash . fst). zip nodes $ idx
  where
    nodes = filter (not . Char.isSpace) . toList $ txt
    size = flip div 2 . fromIntegral . length . lines $ txt
    idx = [(y, x) | x <- [size,size-1..(-size)], y <- [-size..size]]
    isHash x = x == '#'
