module Main (main) where

import Data.List ((\\))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

data Tree a = Tree a [Tree a] deriving (Show)

type Name = String
type Weight = Int

data Program = Program Name Weight [Name] deriving (Show)

instance Eq Program where
    (Program name1 _ _) == (Program name2 _ _) = name1 == name2

main :: IO ()
main = do
    Right programs <- parseInput <$> getContents

    let [rootName] = allPrograms programs \\ allChildren programs

    putStrLn rootName

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
