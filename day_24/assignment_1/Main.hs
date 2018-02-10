{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ClassyPrelude
import qualified Data.Text.Lazy as T
import qualified Data.Word as Word
import Prelude ()
import qualified Data.Vector as V

data Link = Link { _start :: !Word.Word32, _end :: !Word.Word32 }
  deriving (Show, Eq)

main :: IO ()
main = do
    links <- parseInput <$> getContents

    print . maxPath (Link 0 0) . V.fromList $ links

maxPath :: Link -> V.Vector Link -> Word.Word32
maxPath (Link s e) links = case fromNullable (map (uncurry maxPath) legalLinks) of
    Nothing -> s + e
    Just legals -> s + e + maximum legals
  where
    legalLinks = filter legalNext . map flipLegal $ extractAll links
    legalNext (Link s' e', _) = s' == e || e' == e
    flipLegal (Link s' e', vs)
        | s' /= e = (Link e' s', vs)
        | otherwise = (Link s' e', vs)

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
