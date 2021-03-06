{- The bridge you've built isn't long enough; you can't jump the rest of the
 - way.
 -
 - In the example above, there are two longest bridges:
 -
 -  * 0/2--2/2--2/3--3/4
 -  * 0/2--2/2--2/3--3/5
 -
 - Of them, the one which uses the 3/5 component is stronger; its strength is
 - 0+2 + 2+2 + 2+3 + 3+5 = 19.
 -
 - What is the strength of the longest bridge you can make? If you can make
 - multiple bridges of the longest length, pick the strongest one. -}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ClassyPrelude
import qualified Data.List.Extra as L
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import qualified Data.Word as Word
import Prelude ()
import qualified System.Exit as Sys

data Link = Link { _start :: !Word.Word32, _end :: !Word.Word32 }
  deriving (Show, Eq)

main :: IO ()
main = do
    links <- parseInput <$> getContents

    let strengths = maybe [] (map evalBridge) . headMay . L.groupOn length .
            reverse . L.sortOn length . paths (Link 0 0) . V.fromList $ links

    case fromNullable strengths of
        Nothing -> Sys.die "No legal paths"
        Just s -> print . maximum $ s

paths :: Link -> V.Vector Link -> [[Link]]
paths (Link s e) links = case legalLinks of
    [] -> [[Link s e]]
    xs -> map (Link s e:) . concat $ map (uncurry paths) xs
  where
    legalLinks = filter legalNext . map flipLegal $ extractAll links
    legalNext (Link s' e', _) = s' == e || e' == e
    flipLegal (Link s' e', vs)
        | s' /= e = (Link e' s', vs)
        | otherwise = (Link s' e', vs)

evalBridge :: [Link] -> Word.Word32
evalBridge = sum . map evalLink
  where
    evalLink (Link s e) = s + e

parseInput :: LText -> [Link]
parseInput = mapMaybe bridge . lines
  where
    bridge line = case T.splitOn "/" line of
        [a, b] -> do
            c <- readMay a
            d <- readMay b

            return $ Link c d
        _ -> Nothing

extractAll :: V.Vector a -> [(a, V.Vector a)]
extractAll xs = foldr extractVal [] [1..V.length xs]
  where
    extractVal i ys = case V.splitAt i xs of
        (starts, ends) -> (V.last starts, V.concat [V.init starts, ends]):ys
