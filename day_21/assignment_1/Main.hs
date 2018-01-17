{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import ClassyPrelude
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Algorithms.Matrix as R
import qualified Data.Array.Repa.Repr.Unboxed as R
import qualified Data.List as L
import qualified Data.List.Split as L
import Prelude ()
import qualified System.Exit as Sys
import qualified Text.Parsec as P

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right rules -> do
            expandedRules <- expandRules rules

            art <- R.computeP $ createArt 5 expandedRules startArray ::
                IO (R.Array R.U R.DIM2 Bool)

            trueVals <- R.sumAllP $ R.map fromBool art
            print trueVals
  where
    startArray = R.fromListUnboxed (R.Z :. (3::Int) :. (3::Int))
        [ False, True , False
        , False, False, True
        , True , True , True
        ]

    fromBool :: Bool -> Int
    fromBool False = 0
    fromBool True = 1

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
    ((R.Z :. xSize) :. ySize) = R.extent arr

    splitSize = case xSize `mod` 2 of
        0 -> 2
        _ -> 3

    starts = R.ix2 <$> [0,splitSize..(xSize - splitSize)] <*>
        [0,splitSize..(xSize - splitSize)]

    sideLen = xSize `div` splitSize

    extractSize = ((R.Z :. splitSize) :. splitSize)

{- | Replace all arrays with replacements from the rules. -}
replace :: (Eq e, Unbox e) => [Rule e]
    -- ^ Rules for replacements.
    -> [[R.Array R.D R.DIM2 e]]
    -- ^ Arrays to replace.
    -> [[R.Array R.D R.DIM2 e]]
replace rules arrays = map (map (applyRules rules)) arrays

{- | Horizontal and vertical stack of the lists to a single array. -}
collect :: [[R.Array R.D R.DIM2 e]] -> R.Array R.D R.DIM2 e
collect arrays = R.transpose $ vstacked
  where
    vstacked = L.foldl1 R.append . map R.transpose $ hstacked
    hstacked = map (L.foldl1 R.append) arrays

data Rule e = Rule
    { _before :: R.Array R.U R.DIM2 e
    , _after  :: R.Array R.U R.DIM2 e
    }
  deriving Show

{- | Expand all rules with their rotated counterparts. -}
expandRules :: (Monad m, R.Unbox e) => [Rule e] -> m [Rule e]
expandRules rules = concat <$> mapM expandRule rules

{- | Expand a single rule to its rotated counterparts. -}
expandRule :: (Monad m, R.Unbox e) => Rule e -> m [Rule e]
expandRule (Rule arr0 e) = do
    arr1 <- R.computeP $ rotate arr0
    arr2 <- R.computeP $ rotate arr1
    arr3 <- R.computeP $ rotate arr2
    arr4 <- R.computeP $ flipVertical arr0
    arr5 <- R.computeP $ flipVertical arr1
    arr6 <- R.computeP $ flipVertical arr2
    arr7 <- R.computeP $ flipVertical arr3

    return
        [ Rule arr0 e
        , Rule arr1 e
        , Rule arr2 e
        , Rule arr3 e
        , Rule arr4 e
        , Rule arr5 e
        , Rule arr6 e
        , Rule arr7 e
        ]

{- | Apply the first matching rule in the list of rules. -}
applyRules :: (Eq e, R.Source r1 e, R.Unbox e) => [Rule e]
    -- ^ Rules to apply.
    -> R.Array r1 R.DIM2 e
    -- ^ Array to apply rules to.
    -> R.Array R.D R.DIM2 e
applyRules rules arr = case L.find (R.equalsS arr . _before) rules of
    Just (Rule _ after) -> R.delay after
    Nothing -> error $ "No rule found\n"

{- | Rotate an array 90 degrees to the right. -}
rotate :: R.Source r1 e => R.Array r1 R.DIM2 e -> R.Array R.D R.DIM2 e
rotate arr = if xSize /= ySize
    then error "Cannot rotate non square matrix"
    else arr'
  where
    ((R.Z :. xSize) :. ySize) = R.extent arr
    arr' = R.fromFunction (R.Z :. xSize :. ySize) $
        (\(R.Z :. ix :. iy) -> arr R.! (R.Z :. xSize - iy - 1 :. ix))

flipVertical :: R.Source r e => R.Array r R.DIM2 e -> R.Array R.D R.DIM2 e
flipVertical arr = R.traverse arr id flip
  where
    flip f ((R.Z :. x) :. y) = f (R.ix2 (xSize - x - 1) y)
    ((R.Z :. xSize) :. _) = R.extent arr

{- | Parse list of rules from the input. -}
parseInput :: LText -> Either P.ParseError [Rule Bool]
parseInput = P.parse (ruleP `P.endBy` P.char '\n' <* P.eof) ""

ruleP :: P.Parsec LText () (Rule Bool)
ruleP = do
    before <- concat <$> (P.many value) `P.sepBy` P.char '/'
    P.string " => "
    after <- concat <$> (P.many value) `P.sepBy` P.char '/'

    let blen = round . sqrt . fromIntegral $ length before
        alen = round . sqrt . fromIntegral $ length after

        beforeMatrix = R.fromListUnboxed (R.Z :. blen :. blen) before
        afterMatrix = R.fromListUnboxed (R.Z :. alen :. alen) after

    return $ Rule beforeMatrix afterMatrix
  where
    value = P.choice [P.char '#' *> pure True, P.char '.' *> pure False]
