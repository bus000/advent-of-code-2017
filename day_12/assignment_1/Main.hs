{- Walking along the memory banks of the stream, you find a small village that
 - is experiencing a little confusion: some programs can't communicate with each
 - other.
 -
 - Programs in this village communicate using a fixed system of pipes. Messages
 - are passed between programs using these pipes, but most programs aren't
 - connected to each other directly. Instead, programs pass messages between
 - each other until the message reaches the intended recipient.
 -
 - For some reason, though, some of these messages aren't ever reaching their
 - intended recipient, and the programs suspect that some pipes are missing.
 - They would like you to investigate.
 -
 - You walk through the village and record the ID of each program and the IDs
 - with which it can communicate directly (your puzzle input). Each program has
 - one or more programs with which it can communicate, and these pipes are
 - bidirectional; if 8 says it can communicate with 11, then 11 will say it can
 - communicate with 8.
 -
 - You need to figure out how many programs are in the group that contains
 - program ID 0.
 -
 - For example, suppose you go door-to-door like a travelling salesman and
 - record the following list:
 -
 - 0 <-> 2
 - 1 <-> 1
 - 2 <-> 0, 3, 4
 - 3 <-> 2, 4
 - 4 <-> 2, 3, 6
 - 5 <-> 6
 - 6 <-> 4, 5
 -
 - In this example, the following programs are in the group that contains
 - program ID 0:
 -
 -  * Program 0 by definition.
 -  * Program 2, directly connected to program 0.
 -  * Program 3 via program 2.
 -  * Program 4 via program 2.
 -  * Program 5 via programs 6, then 4, then 2.
 -  * Program 6 via programs 4, then 2.
 -
 - Therefore, a total of 6 programs are in this group; all but program 1, which
 - has a pipe that connects it to itself.
 -
 - How many programs are in the group that contains program ID 0? -}
module Main (main) where

import qualified Control.Monad.Loops as C
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set.Monad as Set
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

type Pipes = (Int, [Int])

main :: IO ()
main = do
    input <- parseInput <$> getContents

    case input of
        Left err -> Sys.die $ show err
        Right lgraph -> do
            let initialGraph = fromListEmpty lgraph
            print $ Set.size (reachable initialGraph 0)

parseInput :: String -> Either P.ParseError [Pipes]
parseInput = P.parse (pipes <* P.eof) ""

type InputParser a = P.Parsec String () a

pipes :: InputParser [Pipes]
pipes = pipe `P.endBy` P.char '\n'

pipe :: InputParser Pipes
pipe = (,) <$> P.int <* P.string " <-> " <*> P.int `P.sepBy1` P.string ", "

{- Graph specific types and functions. -}

data Graph a b = Graph (Map.Map a (Node a b)) deriving (Show)
data Node a b = Node b [a] deriving (Show)

emptyGraph :: Graph a b
emptyGraph = Graph Map.empty

fromList :: Ord a => [(a, b, [a])] -> Graph a b
fromList = foldr consGraph emptyGraph
  where
    consGraph (key, value, ns) (Graph g) =
        Graph $ Map.insert key (Node value ns) g

fromListEmpty :: Ord a => [(a, [a])] -> Graph a ()
fromListEmpty = fromList . map (\(x, y) -> (x, (), y))

reachable :: Ord a => Graph a b -> a -> Set.Set a
reachable graph a =
    fst $ S.execState (visit `C.untilM_` toVisitEmpty) initialState
  where
    initialState = (Set.empty, Set.singleton a)

    visit = do
        (visited, toVisit) <- S.get
        let allNeighbours = toVisit >>= neighbours graph
            visited' = Set.union visited toVisit
            toVisit' = Set.filter (`Set.notMember` visited') allNeighbours
        S.put (visited', toVisit')

    toVisitEmpty = do
        (_, toVisit) <- S.get
        return $ Set.null toVisit

neighbours :: Ord a => Graph a b -> a -> Set.Set a
neighbours graph a = case graphLookup graph a of
    Just (Node _ ns) -> Set.fromList ns
    Nothing -> Set.empty

graphLookup :: Ord a => Graph a b -> a -> Maybe (Node a b)
graphLookup (Graph g) a = Map.lookup a g
