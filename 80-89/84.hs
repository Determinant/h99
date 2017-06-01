import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl', sortBy)
import Control.Monad (foldM, mapM)

data Graph a b = Graph [a] [(a, a, b)] deriving (Show, Eq)
                
graph = Graph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),
                           (2,5,32),(3,4,61),(3,5,44),(4,5,93)]

newElem :: Ord a => Map a a -> a -> Maybe (Map a a)
findLead :: Ord a => Map a a -> a -> Maybe (Map a a, a)
unionSet :: Ord a => Map a a -> a -> a -> Maybe (Map a a)

newElem m v = Just (Map.insert v v m)
findLead m v = do pv <- Map.lookup v m
                  if pv == v then return (m, v) else do
                    (m', v') <- findLead m pv
                    return (Map.adjust (\_ -> v') v m', v')
unionSet m u v = do (m', lu) <- findLead m u
                    (m'', lv) <- findLead m' v
                    return (Map.adjust (\_ -> lv) lu m'')

-- The above lines implement a monadic union-find-set, e.g:
-- z = do m <- newElem Map.empty 1
--        m1 <- newElem m 2
--        m2 <- newElem m1 3
--        m3 <- newElem m2 4
--        m4 <- unionSet m3 1 2
--        m5 <- unionSet m4 3 4
--        m6 <- unionSet m5 2 3
--        (m7, _) <- findLead m6 1
--        return m7

kruskal :: (Ord a, Ord b, Num b) => Graph a b -> Maybe b

kruskal (Graph [] _) = Nothing
kruskal (Graph v e) = span (sortBy (\(_, _, c) (_, _, c') -> c `compare` c') e)
                           (foldM (\acc u -> newElem acc u) Map.empty v) 0
    where span [] comp0 cost = return cost
          span ((u, v, c):es) comp0 cost =
              do comp <- comp0
                 (comp', lu) <- findLead comp u
                 (comp'', lv) <- findLead comp' v
                 comp''' <- unionSet comp'' u v
                 let (compf, cost') = if lu == lv then (comp'', cost)
                                                  else (comp''', cost + c)
                 span es (return compf) cost'
