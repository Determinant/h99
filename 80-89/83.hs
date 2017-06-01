import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Control.Monad (foldM, mapM)

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
                
k4 = Graph ['a', 'b', 'c', 'd']
     [('a', 'b'), ('b', 'c'), ('c', 'd'),
     ('d', 'a'), ('a', 'c'), ('b', 'd')]

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

spantree :: Ord a => Graph a -> Maybe Int

spantree (Graph v e) = span e $ foldM (\acc u -> newElem acc u) Map.empty v
    where span [] comp0 = do comp <- comp0
                             lu <- mapM (\u -> do (_, l) <- findLead comp u
                                                  return l) v
                             return (if all (== head lu) lu then 1 else 0)
          span ((u, v):es) comp0 = do comp <- comp0
                                      (comp', lu) <- findLead comp u
                                      (comp'', lv) <- findLead comp' v
                                      l <- span es $ return comp
                                      if lu == lv then return l else do
                                         comp''' <- unionSet comp'' u v
                                         r <- span es $ return comp'''
                                         return (l + r)
