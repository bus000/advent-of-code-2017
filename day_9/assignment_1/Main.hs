module Main (main) where

import qualified Text.Parsec as P
import qualified System.Exit as S

data Group = Group [Group] deriving (Show)

main :: IO ()
main = do
    input <- parseInput . init <$> getContents

    case input of
        Left err -> S.die $ show err
        Right groups -> print $ getScore 1 groups

getScore :: Int -> Group -> Int
getScore level (Group groups) = level + sum (map (getScore (level + 1)) groups)

parseInput :: String -> Either P.ParseError Group
parseInput = P.parse (group <* P.eof) ""

type GroupParser a = P.Parsec String () a

group :: GroupParser Group
group = Group <$> P.between (P.char '{') (P.char '}') groupContent

groupContent :: GroupParser [Group]
groupContent = P.skipMany anyGarbage *> P.sepEndBy group (P.many anyGarbage)
  where
    anyGarbage = P.choice [ignore bangIgnore, ignore garbage, ignore groupChar]
    groupChar = P.noneOf "{}"
    ignore x = x *> pure ()

garbage :: GroupParser String
garbage = P.between (P.char '<') (P.char '>') garbageContent

garbageContent :: GroupParser String
garbageContent = P.many $ P.choice [bangIgnore, garbageChar]
  where
    garbageChar = P.noneOf ">"

bangIgnore :: GroupParser Char
bangIgnore = P.char '!' *> P.anyChar
