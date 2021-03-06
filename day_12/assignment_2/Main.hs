{- There are more programs than just the ones in the group containing program ID
 - 0. The rest of them have no way of reaching that group, and still might have
 - no way of reaching each other.
 -
 - A group is a collection of programs that can all communicate via pipes either
 - directly or indirectly. The programs you identified just a moment ago are all
 - part of the same group. Now, they would like you to determine the total
 - number of groups.
 -
 - In the example above, there were 2 groups: one consisting of programs
 - 0,2,3,4,5,6, and the other consisting solely of program 1.
 -
 - How many groups are there in total? -}
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
            print $ length (connectedComponentRoots initialGraph)

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

connectedComponentRoots :: Ord a => Graph a b -> [a]
connectedComponentRoots graph =
    snd $ foldr connectedComponent (Set.empty, []) (graphKeys graph)
  where
    connectedComponent a (visited, roots)
        | a `Set.member` visited = (visited, roots)
        | otherwise = (Set.union visited (reachable graph a), a:roots)

neighbours :: Ord a => Graph a b -> a -> Set.Set a
neighbours graph a = case graphLookup graph a of
    Just (Node _ ns) -> Set.fromList ns
    Nothing -> Set.empty

graphLookup :: Ord a => Graph a b -> a -> Maybe (Node a b)
graphLookup (Graph g) a = Map.lookup a g

graphKeys :: Graph a b -> [a]
graphKeys (Graph g) = Map.keys g
