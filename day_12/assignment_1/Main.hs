module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified System.Exit as Sys
import qualified Control.Monad.State as S
import qualified Control.Monad.Loops as C

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
pipes = pipe `P.endBy` (P.char '\n')

pipe :: InputParser Pipes
pipe = (,) <$> P.int <* P.string " <-> " <*> P.int `P.sepBy1` (P.string ", ")

{- Graph specific types and functions. -}

data Graph a b = Graph (Map.Map a (Node a b)) deriving (Show)
data Node a b = Node b [a] deriving (Show)

emptyGraph :: Graph a b
emptyGraph = Graph Map.empty

fromList :: Ord a => [(a, b, [a])] -> Graph a b
fromList = foldr consGraph emptyGraph
  where
    consGraph (key, value, neighbours) (Graph g) =
        Graph $ Map.insert key (Node value neighbours) g

fromListEmpty :: Ord a => [(a, [a])] -> Graph a ()
fromListEmpty = fromList . map (\(x, y) -> (x, (), y))

reachable :: Ord a => Graph a b -> a -> Set.Set a
reachable graph a =
    fst $ S.execState (visitFirst `C.untilM_` toVisitEmpty) initialState
  where
    initialState = (Set.empty, Set.singleton a)

    visitFirst = do
        (visited, toVisit) <- S.get
        case Set.maxView toVisit of
            Just (v, vs) ->
                let ns = neighbours graph v
                    unseen = filter (`Set.notMember` visited) ns
                    toVisit' = Set.union vs (Set.fromList unseen)
                in S.put (Set.insert v visited, toVisit')
            Nothing -> S.put (visited, toVisit)

    toVisitEmpty = do
        (_, toVisit) <- S.get
        return $ Set.null toVisit

neighbours :: Ord a => Graph a b -> a -> [a]
neighbours graph a = case graphLookup graph a of
    Just (Node _ ns) -> ns
    Nothing -> []

graphLookup :: Ord a => Graph a b -> a -> Maybe (Node a b)
graphLookup (Graph g) a = Map.lookup a g
