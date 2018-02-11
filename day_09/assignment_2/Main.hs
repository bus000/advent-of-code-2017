{- Now, you're ready to remove the garbage.
 -
 - To prove you've removed it, you need to count all of the characters within
 - the garbage. The leading and trailing < and > don't count, nor do any
 - canceled characters or the ! doing the canceling.
 -
 -  * <>, 0 characters.
 -  * <random characters>, 17 characters.
 -  * <<<<>, 3 characters.
 -  * <{!>}>, 2 characters.
 -  * <!!>, 0 characters.
 -  * <!!!>>, 0 characters.
 -  * <{o"i!a,<{i<a>, 10 characters.
 -
 - How many non-canceled characters are within the garbage in your puzzle
 - input? -}
module Main (main) where

import qualified System.Exit as S
import qualified Text.Parsec as P

newtype Group = Group [Either Garbage Group] deriving (Show)
newtype Garbage = Garbage String deriving (Show)

main :: IO ()
main = do
    input <- parseInput . init <$> getContents

    case input of
        Left err -> S.die $ show err
        Right g -> print (length $ concatMap garbageString (getGarbage g))

getGarbage :: Group -> [Garbage]
getGarbage (Group xs) = concatMap getGarbage' xs
  where
    getGarbage' (Right g) = getGarbage g
    getGarbage' (Left g) = [g]

garbageString :: Garbage -> String
garbageString (Garbage g) = g

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
