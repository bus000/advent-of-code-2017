module Main (main) where

import Data.List ((\\), find, nub)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad as C
import Data.Monoid (Sum(..), getSum)

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
        tree = Sum <$> buildTree programs root
        treeS = getSum <$> treeSum tree
        Just (Tree value branches) = findInbalance treeS

    print (value, map getTreeValue branches)

buildTree :: [Program] -> Program -> Tree Weight
buildTree programs root@(Program _ weight children) =
    Tree weight (map (buildTree programs) childPrograms)
  where
    childPrograms = filter (\x -> getName x `elem` children) programs

treeSum :: Monoid a => Tree a -> Tree a
treeSum (Tree value branches) = Tree (mconcat $ value:newValues) newBranches
  where
    newBranches = map treeSum branches
    newValues = map getTreeValue newBranches

findInbalance :: Ord a => Tree a -> Maybe (Tree a)
findInbalance tree@(Tree value branches)
    | not (isBalanced tree) && all isBalanced branches = Just tree
    | otherwise = C.msum $ map findInbalance branches

isBalanced :: Ord a => Tree a -> Bool
isBalanced (Tree _ branches) = length unique <= 1
  where
    unique = nub $ map getTreeValue branches

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

getTreeValue :: Tree a -> a
getTreeValue (Tree x _) = x
