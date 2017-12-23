module Main (main) where

import qualified Data.Bits as Bits
import qualified Data.Int as Int

main :: IO ()
main = do
    [gena, genb] <- map (read . last . words) . lines <$> getContents

    print $ length . filter compareLowest . take 5000000 $ anbs gena genb

compareLowest :: (Int.Int32, Int.Int32) -> Bool
compareLowest (a, b) = Bits.shift a 16 == Bits.shift b 16

anbs :: Integer -> Integer -> [(Int.Int32, Int.Int32)]
anbs a b = zip (map fromIntegral $ as a) (map fromIntegral $ bs b)

as :: Integer -> [Integer]
as = drop 1 . filter (\x -> x `mod` 4 == 0) . iterate f
  where
    f x = (x * 16807) `mod` 2147483647

bs :: Integer -> [Integer]
bs = drop 1 . filter (\x -> x `mod` 8 == 0) . iterate f
  where
    f x = (x * 48271) `mod` 2147483647
