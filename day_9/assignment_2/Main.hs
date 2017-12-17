module Main (main) where

import qualified Text.Parsec as P
import qualified System.Exit as S
import qualified Data.Either as E

data Group = Group [Either Garbage Group] deriving (Show)
data Garbage = Garbage String deriving (Show)

main :: IO ()
main = do
    input <- parseInput . init <$> getContents

    case input of
        Left err -> S.die $ show err
        Right group ->
            print (length $ concatMap garbageString (getGarbage group))

getGarbage :: Group -> [Garbage]
getGarbage (Group xs) = concatMap getGarbage' xs
  where
    getGarbage' (Right group) = getGarbage group
    getGarbage' (Left garbage) = [garbage]

garbageString :: Garbage -> String
garbageString (Garbage garbage) = garbage

parseInput :: String -> Either P.ParseError Group
parseInput = P.parse (group <* P.eof) ""

type GroupParser a = P.Parsec String () a

group :: GroupParser Group
group = Group <$> P.between (P.char '{') (P.char '}') groupContent

groupContent :: GroupParser [Either Garbage Group]
groupContent = P.skipMany anyGarbage *> P.sepEndBy content (P.many anyGarbage)
  where
    content = P.choice [Left <$> garbage, Right <$> group]
    anyGarbage = P.choice [ignore bangIgnore, ignore groupChar]
    groupChar = P.noneOf "{}<"
    ignore x = x *> pure ()

garbage :: GroupParser Garbage
garbage = Garbage <$> P.between (P.char '<') (P.char '>') garbageContent

garbageContent :: GroupParser String
garbageContent = P.skipMany bangIgnore *> P.sepEndBy garbageChar (P.many bangIgnore)
  where
    garbageChar = P.noneOf "!>"

bangIgnore :: GroupParser Char
bangIgnore = P.char '!' *> P.anyChar
