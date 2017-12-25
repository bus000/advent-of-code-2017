module Main (main) where

import qualified Data.Sequence as Seq
import qualified System.Exit as Sys

main :: IO ()
main = do
    input <- read <$> getContents

    let initialLock = Spinlock (Seq.singleton 0) 0 input
        Spinlock endLock _ _ = insertAll initialLock [1..2017]

    case Seq.elemIndexL (2017 :: Int) endLock of
        Just i -> print $ Seq.index endLock (i+1)
        Nothing -> Sys.die "Could not find 2017"

type Position = Int
type ShiftSize = Int

data Spinlock a = Spinlock (Seq.Seq a) Position ShiftSize deriving (Show)

insertAll :: Show a => Spinlock a -> [a] -> Spinlock a
insertAll lock = foldl insert lock

insert :: Spinlock a -> a -> Spinlock a
insert lock x = addValue (move lock) x

move :: Spinlock a -> Spinlock a
move (Spinlock lock pos shift) = Spinlock lock pos' shift
  where
    pos' = head . drop shift $ positions
    positions = drop pos . cycle $ [0..Seq.length lock - 1]

addValue :: Spinlock a -> a -> Spinlock a
addValue (Spinlock lock pos shift) x = Spinlock lock' pos' shift
  where
    pos' = pos + 1
    lock' = Seq.insertAt pos' x lock
