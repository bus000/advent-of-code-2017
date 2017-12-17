module Main (main) where

import Data.List ((\\), find, nub, sortOn)
import Data.List.Extra (groupOn)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad as C
import Data.Monoid (Sum(..), getSum)
import Control.Arrow ((&&&))

data Tree a = Tree a [Tree a] deriving (Show)

instance Functor Tree where
    fmap f (Tree a branches) = Tree (f a) (map (fmap f) branches)

type Name = String
type Weight = Int

data Program = Program Name Weight [Name] deriving (Show)

instance Eq Program where
    (Program name1 _ _) == (Program name2 _ _) = name1 == name2

main :: IO ()
main = do
    Right programs <- parseInput <$> getContents

    let [rootName] = allPrograms programs \\ allChildren programs
        Just root = find ((==) rootName . getName) programs
        tree = buildTree programs root
        weights = fmap getSum (treeSum $ Sum . getWeight <$> tree)
        Just (Tree _ branches) = findInbalance $ combine tree weights
        ts = map (((getWeight . fst) &&& snd) . getTreeValue) branches
        [(a,b), (_,d)] = map head . sortOn length . groupOn snd . sortOn snd $ ts

    print $ a - abs (b - d)

buildTree :: [Program] -> Program -> Tree Program
buildTree programs root@(Program _ _ children) =
    Tree root (map (buildTree programs) childPrograms)
  where
    childPrograms = filter (\x -> getName x `elem` children) programs

treeSum :: Monoid a => Tree a -> Tree a
treeSum (Tree value branches) = Tree (mconcat $ value:newValues) newBranches
  where
    newBranches = map treeSum branches
    newValues = map getTreeValue newBranches

findInbalance :: Tree (Program, Weight) -> Maybe (Tree (Program, Weight))
findInbalance tree@(Tree (_, _) branches)
    | not (isBalanced tree) && all isBalanced branches = Just tree
    | otherwise = C.msum $ map findInbalance branches

combine :: Tree a -> Tree b -> Tree (a, b)
combine (Tree a as) (Tree b bs) = Tree (a, b) newBranches
  where
    newBranches = zipWith combine as bs

isBalanced :: Tree (Program, Weight) -> Bool
isBalanced (Tree _ branches) = length unique <= 1
  where
    unique = nub $ map (snd . getTreeValue) branches

allChildren :: [Program] -> [Name]
allChildren = concatMap getChildren

allPrograms :: [Program] -> [Name]
allPrograms = map getName

parseInput :: String -> Either P.ParseError [Program]
parseInput = P.parse (P.many program <* P.eof) ""

type ProgramParser a = P.Parsec String () a

program :: ProgramParser Program
program = Program <$>
    name <* P.space <*>
    P.between (P.char '(') (P.char ')') P.nat <*>
    P.option [] (P.string " -> " *> P.sepBy1 name (P.string ", ")) <* P.newline
  where
    name = P.many $ P.noneOf ", \n"

getChildren :: Program -> [Name]
getChildren (Program _ _ children) = children

getName :: Program -> Name
getName (Program name _ _) = name

getWeight :: Program -> Weight
getWeight (Program _ weight _) = weight

getTreeValue :: Tree a -> a
getTreeValue (Tree x _) = x
