{- You find a program trying to generate some art. It uses a strange process
 - that involves repeatedly enhancing the detail of an image through a set of
 - rules.
 -
 - The image consists of a two-dimensional square grid of pixels that are either
 - on (#) or off (.). The program always begins with this pattern:
 -
 - .#.
 - ..#
 - ###
 -
 - Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to
 - have a size of 3.
 -
 - Then, the program repeats the following process:
 -
 -  * If the size is evenly divisible by 2, break the pixels up into 2x2
 -    squares, and convert each 2x2 square into a 3x3 square by following the
 -    corresponding enhancement rule.
 -  * Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3
 -    squares, and convert each 3x3 square into a 4x4 square by following the
 -    corresponding enhancement rule.
 -  * Because each square of pixels is replaced by a larger one, the image gains
 -    pixels and so its size increases.
 -
 - The artist's book of enhancement rules is nearby (your puzzle input);
 - however, it seems to be missing rules. The artist explains that sometimes,
 - one must rotate or flip the input pattern to find a match. (Never rotate or
 - flip the output pattern, though.) Each pattern is written concisely: rows are
 - listed as single units, ordered top-down, and separated by slashes. For
 - example, the following rules correspond to the adjacent patterns:
 -
 - ../.#  =  ..
 -           .#
 -
 -                 .#.
 - .#./..#/###  =  ..#
 -                 ###
 -
 -                         #..#
 - #..#/..../#..#/.##.  =  ....
 -                         #..#
 -                         .##.
 -
 - When searching for a rule to use, rotate and flip the pattern as necessary.
 - For example, all of the following patterns match the same rule:
 -
 - .#.   .#.   #..   ###
 - ..#   #..   #.#   ..#
 - ###   ###   ##.   .#.
 -
 - Suppose the book contained the following two rules:
 -
 - ../.# => ##./#../...
 - .#./..#/### => #..#/..../..../#..#
 -
 - As before, the program begins with this pattern:
 -
 - .#.
 - ..#
 - ###
 -
 - The size of the grid (3) is not divisible by 2, but it is divisible by 3. It
 - divides evenly into a single square; the square matches the second rule,
 - which produces:
 -
 - #..#
 - ....
 - ....
 - #..#
 -
 - The size of this enhanced grid (4) is evenly divisible by 2, so that rule is
 - used. It divides evenly into four squares:
 -
 - #.|.#
 - ..|..
 - --+--
 - ..|..
 - #.|.#
 -
 - Each of these squares matches the same rule (../.# => ##./#../...), three of
 - which require some flipping and rotation to line up with the rule. The output
 - for the rule is the same in all four cases:
 -
 - ##.|##.
 - #..|#..
 - ...|...
 - ---+---
 - ##.|##.
 - #..|#..
 - ...|...
 -
 - Finally, the squares are joined into a new grid:
 -
 - ##.##.
 - #..#..
 - ......
 - ##.##.
 - #..#..
 - ......
 -
 - Thus, after 2 iterations, the grid contains 12 pixels that are on.
 -
 - How many pixels stay on after 5 iterations?
 -
 - Your puzzle answer was 194.
 -
 - --- Part Two ---
 - How many pixels stay on after 18 iterations? -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Repr.Unboxed as R
import qualified Data.Bool as B
import qualified Data.List as L
import qualified Data.List.Split as L
import Prelude ()
import qualified System.Exit as Sys
import qualified Text.Parsec as P

main :: IO ()
main = do
    rules <- parseInput <$> getContents >>= either (Sys.die . show) return

    let expandedRules = expandRules rules
        art = createArt 5 expandedRules startArray

    print =<< R.sumAllP (R.map (B.bool (0::Int) (1::Int)) art)
  where
    startArray = R.fromListUnboxed (R.Z :. (3::Int) :. (3::Int))
        [ False, True , False
        , False, False, True
        , True , True , True
        ]

{- | Continuously replace subarrays of size 2x2 or 3x3 with arrays from the
 - list of rules. Runs the number of steps given. -}
createArt :: (Eq e, R.Source r1 e, R.Unbox e) => Int
    -- ^ Number of steps to take.
    -> [Rule e]
    -- ^ List of rules to apply.
    -> R.Array r1 R.DIM2 e
    -- ^ Start array.
    -> R.Array R.D R.DIM2 e
createArt steps rules start = L.iterate artStep (R.delay start) L.!! steps
  where
    artStep = collect . replace rules . split

{- | Split array into smaller arrays of either size 2x2 or size 3x3. -}
split :: R.Array R.D R.DIM2 e
    -- ^ Array to split into pieces.
    -> [[R.Array R.D R.DIM2 e]]
split arr = L.chunksOf sideLen $
    map (\start -> R.extract start extractSize arr) starts
  where
    ((R.Z :. xSize) :. _) = R.extent arr

    splitSize = case xSize `mod` 2 of
        0 -> 2
        _ -> 3

    starts = R.ix2 <$> [0,splitSize..(xSize - splitSize)] <*>
        [0,splitSize..(xSize - splitSize)]

    sideLen = xSize `div` splitSize

    extractSize = (R.Z :. splitSize) :. splitSize

{- | Replace all arrays with replacements from the rules. -}
replace :: (Eq e, Unbox e) => [Rule e]
    -- ^ Rules for replacements.
    -> [[R.Array R.D R.DIM2 e]]
    -- ^ Arrays to replace.
    -> [[R.Array R.D R.DIM2 e]]
replace rules = map (map (applyRules rules))

{- | Horizontal and vertical stack of the lists to a single array. -}
collect :: [[R.Array R.D R.DIM2 e]] -> R.Array R.D R.DIM2 e
collect arrays = R.transpose vstacked
  where
    vstacked = L.foldl1 R.append . map R.transpose $ hstacked
    hstacked = map (L.foldl1 R.append) arrays

data Rule e = Rule
    { _before :: R.Array R.U R.DIM2 e
    , _after  :: R.Array R.U R.DIM2 e
    }
  deriving Show

{- | Expand all rules with their rotated counterparts. -}
expandRules :: R.Unbox e => [Rule e] -> [Rule e]
expandRules = concatMap expandRule

{- | Expand a single rule to its rotated counterparts. -}
expandRule :: R.Unbox e => Rule e -> [Rule e]
expandRule (Rule before0 after) = map (`Rule` after)
    [before0, before1, before2, before3, before4, before5, before6, before7]
  where
    before1 = R.computeS $ rotate before0
    before2 = R.computeS $ rotate before1
    before3 = R.computeS $ rotate before2
    before4 = R.computeS $ flipVertical before0
    before5 = R.computeS $ flipVertical before1
    before6 = R.computeS $ flipVertical before2
    before7 = R.computeS $ flipVertical before3

{- | Apply the first matching rule in the list of rules. -}
applyRules :: (Eq e, R.Source r1 e, R.Unbox e) => [Rule e]
    -- ^ Rules to apply.
    -> R.Array r1 R.DIM2 e
    -- ^ Array to apply rules to.
    -> R.Array R.D R.DIM2 e
applyRules rules arr = case L.find (R.equalsS arr . _before) rules of
    Just (Rule _ after) -> R.delay after
    Nothing -> error "No rule found\n"

{- | Rotate an array 90 degrees to the right. -}
rotate :: R.Source r1 e => R.Array r1 R.DIM2 e -> R.Array R.D R.DIM2 e
rotate arr = if xSize /= ySize
    then error "Cannot rotate non square matrix"
    else arr'
  where
    ((R.Z :. xSize) :. ySize) = R.extent arr
    arr' = R.fromFunction (R.Z :. xSize :. ySize)
        (\(R.Z :. ix :. iy) -> arr R.! (R.Z :. xSize - iy - 1 :. ix))

flipVertical :: R.Source r e => R.Array r R.DIM2 e -> R.Array R.D R.DIM2 e
flipVertical arr = R.traverse arr id flipVert
  where
    flipVert f ((R.Z :. x) :. y) = f (R.ix2 (xSize - x - 1) y)
    ((R.Z :. xSize) :. _) = R.extent arr

{- | Parse list of rules from the input. -}
parseInput :: LText -> Either P.ParseError [Rule Bool]
parseInput = P.parse (ruleP `P.endBy` P.char '\n' <* P.eof) ""

ruleP :: P.Parsec LText () (Rule Bool)
ruleP = do
    before <- concat <$> P.many value `P.sepBy` P.char '/'
    C.void $ P.string " => "
    after <- concat <$> P.many value `P.sepBy` P.char '/'

    let blen = round . sqrt . (fromIntegral :: Int -> Double) $ length before
        alen = round . sqrt . (fromIntegral :: Int -> Double) $ length after

        beforeMatrix = R.fromListUnboxed (R.Z :. blen :. blen) before
        afterMatrix = R.fromListUnboxed (R.Z :. alen :. alen) after

    return $ Rule beforeMatrix afterMatrix
  where
    value = P.choice [P.char '#' *> pure True, P.char '.' *> pure False]
