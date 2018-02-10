{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import ClassyPrelude
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Text.Lazy as T
import qualified Data.Word as Word
import qualified Prelude
import qualified Data.Vector as V
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy as T

data Link a = Link { _start :: !a, _end :: !a }
  deriving (Show, Eq, Functor)

data Tree a = Node a [Tree a] deriving (Show, Functor)

main :: IO ()
main = do
    links <- parseInput <$> getContents

    let linkmap = buildLinkMap (T.take 1 . _start) links
        tree = buildTree "0" (T.singleton . T.last . _end) linkmap
        linknumtree = (Prelude.read . unpack :: LText -> Int) <$$$> tree
        numtree = (\(Link s e) -> s + e) <$$> linknumtree

    print numtree

buildLinkMap :: Ord b => (a -> b) -> [a] -> Map b (V.Vector a)
buildLinkMap f xs = Map.fromAscList $ zip mapkeys mapvals
  where
    mapkeys = L.nub . sort . map f $ xs
    mapvals = map V.fromList . L.groupOn f . L.sortOn f $ xs

buildTree :: Ord b => b -> (a -> b) -> Map.Map b (V.Vector a) -> [Tree a]
buildTree x f xs = map consTree legal
  where
    legal = extractAll $ Map.findWithDefault V.empty x xs
    consTree (x', other) = Node x' $ buildTree (f x') f (Map.insert x other xs)

maxPath :: (Monad m, Num a, Ord a) => Tree a -> m a
maxPath (Node x xs) = do
    subpaths <- mapM maxPath xs

    case fromNullable subpaths of
        Nothing -> return x
        Just subpaths' -> return $ x + maximum subpaths'

parseInput :: LText -> [Link LText]
parseInput = mapMaybe bridge . lines
  where
    bridge line = case T.splitOn "/" line of
        [a, b] -> Just $ Link a b
        _ -> Nothing

extractAll :: V.Vector a -> [(a, V.Vector a)]
extractAll xs = foldr extractVal [] [1..V.length xs]
  where
    extractVal i ys = case V.splitAt i xs of
        (starts, ends) -> (V.last starts, V.concat [V.init starts, ends]):ys

(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a))
    -> f1 (f2 (f3 b))
(<$$$>) = fmap . fmap . fmap

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap
