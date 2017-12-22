module Graph
    {- Constructors. -}
    ( emptyGraph
    , fromList
    , fromListEmpty
    {- Queries. -}
    , reachable
    , connectedComponentRoots
    {- Graph type. -}
    , Graph
    ) where

import qualified Data.Map as Map
import qualified Data.Set.Monad as Set
import qualified Control.Monad.State as S
import qualified Control.Monad.Loops as C

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
