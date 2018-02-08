{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ClassyPrelude
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Text.Lazy as T
import qualified Data.Word as Word
import Prelude ()
import qualified Data.Vector as V
import qualified Data.Map.Lazy as Map

data Link = Link { _start :: !Word.Word32, _end :: !Word.Word32 }
  deriving (Show, Eq)

data Tree a = Node a [Tree a] deriving (Show)

main :: IO ()
main = do
    links <- parseInput <$> getContents

    print $ map maxPath $ buildTree 0 $ buildLinkMap links
    {-mapM_ print $ map allPaths $ buildTree 0 $ buildLinkMap links-}

buildLinkMap :: [Link] -> Map Word.Word32 (V.Vector Link)
buildLinkMap links = Map.fromAscList $ zip mapkeys mapvals
  where
    mapkeys = L.nub . sort . map _start $ links
    mapvals = map V.fromList . L.groupOn _start . L.sortOn _start $ links

buildTree :: Word.Word32 -> Map.Map Word.Word32 (V.Vector Link) -> [Tree Link]
buildTree n links = map consTree legalLinks
  where
    legalLinks = extractAll $ maybe V.empty id (Map.lookup n links)
    consTree (l@(Link _ e), otherlinks) =
        Node l $ buildTree e (Map.insert n otherlinks links)

maxPath :: Tree Link -> Word.Word32
maxPath (Node (Link s e) ts) = case fromNullable (map maxPath ts) of
    Nothing -> s + e
    Just ts' -> s + e + maximum ts'

allPaths :: Tree Link -> [[Link]]
allPaths (Node l []) = [[l]]
allPaths (Node l ls) = map (l:) (concatMap allPaths ls)

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
