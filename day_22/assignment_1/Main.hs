{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.RWS as C
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Word as Word
import Prelude ()
import qualified System.Exit as S

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
burst infected = do
    (position, direction) <- C.get

    -- TODO: Rewrite with case.
    let isInfected = position `member` infected
        direction' = bool (left direction) (right direction) isInfected
        position' = move position direction'

    C.put (position', direction')

    if isInfected
        then return $ deleteSet position infected
        else do
            C.tell 1
            return $ insertSet position infected
  where
    right U = R
    right R = D
    right D = L
    right L = U

    left U = L
    left R = U
    left D = R
    left L = D

    move (x, y) U = (x    , y + 1)
    move (x, y) D = (x    , y - 1)
    move (x, y) L = (x - 1, y    )
    move (x, y) R = (x + 1, y    )

parseInput :: LText -> Infected
parseInput txt = setFromList . map snd . filter (isHash . fst). zip nodes $ idx
  where
    nodes = filter (not . Char.isSpace) . toList $ txt
    size = (flip div) 2 . fromIntegral . length . lines $ txt
    idx = [(y, x) | x <- [size,size-1..(-size)], y <- [-size..size]]
    isHash x = x == '#'
