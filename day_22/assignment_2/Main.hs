{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad.RWS.Strict as C
import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Word as Word
import Prelude ()

type Position = (Integer, Integer)
type Infected = Set Position
type Weakened = Set Position
type Flagged = Set Position
data Cluster = Cluster !Infected !Weakened !Flagged
data Direction = U | D | L | R

main :: IO ()
main = do
    input <- parseInput <$> getContents

    let cluster = Cluster input Set.empty Set.empty

    print . C.getSum . snd =<<
        C.evalRWST (bursts cluster 10000000) () ((0, 0), U)

bursts :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Cluster
    -- ^ Original infected nodes.
    -> Word.Word
    -- ^ Number of bursts to run.
    -> m Cluster
bursts infected n = F.foldrM (\_ inf -> burst inf) infected [1..n]

{- | Perform single burst and return new state. When a node is infected the
 - writer sum is increased by 1. -}
burst :: (C.MonadRWS () (C.Sum Int) (Position, Direction) m) => Cluster
    -- ^ Original infected nodes.
    -> m Cluster
burst (Cluster infected weakened flagged) = turn *> newCluster <* move
  where
    newCluster = C.gets fst >>= \position -> case () of
        _ | position `member` infected ->
            return $ Cluster (deleteSet position infected) weakened
                (insertSet position flagged)
        _ | position `member` weakened -> do
            C.tell 1
            return $ Cluster (insertSet position infected)
                (deleteSet position weakened) flagged
        _ | position `member` flagged ->
            return $ Cluster infected weakened (deleteSet position flagged)
        _ -> return $ Cluster infected (insertSet position weakened) flagged

    turn = C.modify $ \case
        (position, U) -> case () of
            _ | position `member` infected -> (position, R)
            _ | position `member` weakened -> (position, U)
            _ | position `member` flagged -> (position, D)
            _ -> (position, L)

        (position, D) -> case () of
            _ | position `member` infected -> (position, L)
            _ | position `member` weakened -> (position, D)
            _ | position `member` flagged -> (position, U)
            _ -> (position, R)

        (position, L) -> case () of
            _ | position `member` infected -> (position, U)
            _ | position `member` weakened -> (position, L)
            _ | position `member` flagged -> (position, R)
            _ -> (position, D)

        (position, R) -> case () of
            _ | position `member` infected -> (position, D)
            _ | position `member` weakened -> (position, R)
            _ | position `member` flagged -> (position, L)
            _ -> (position, U)

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
    idx = [(y, x) | x <- [size, size-1..(-size)], y <- [-size..size]]
    isHash x = x == '#'
