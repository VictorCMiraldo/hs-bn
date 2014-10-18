{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
module BN.EdgeSet where

import BN.Common

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.Function(on)

import Test.QuickCheck hiding ((><))

--------------------------------------------------------------------------------
-- ** Edge operations

class (Ord e, Ord i) => IsEdge e i | e -> i where
  edgeLimits :: e -> (i, i)
  edgeLimits = split edgeSrc edgeDst
  
  edgeSrc    :: e -> i
  edgeSrc    = fst . edgeLimits
  
  edgeDst    :: e -> i
  edgeDst    = snd . edgeLimits
  
  edgeTouches :: i -> e -> Bool
  edgeTouches v = uncurry (||) . ((== v) >< (== v)) . edgeLimits
  
-- |Trivial instance
instance (Ord i) => IsEdge (i, i) i where
  edgeLimits = id

--------------------------------------------------------------------------------
-- ** The Edge Set

data EdgeSet e i = ES
  { edgesIn  :: M.Map i (S.Set e)
  , edgesOut :: M.Map i (S.Set e)
  } deriving (Show, Eq)

-- |Arbitrary instance, for quickcheck
instance (IsEdge e i, Arbitrary i, Arbitrary e, Enum i) 
  => Arbitrary (EdgeSet e i) where
  arbitrary
    = do
      i <- choose (100, 250) :: Gen Int
      foldM (\r _ -> arbitrary >>= return . (++> r)) esEmpty [0..i] 
      
-- |Returns the empty edge set.  
esEmpty :: (IsEdge e i) => EdgeSet e i
esEmpty = ES M.empty M.empty
      
--------------------------------------------------------------------------------
-- ** Operations  

-- *** Adding and removing data

-- |Given a edge e, inserts it into the given edge set.  
esAddEdge :: (IsEdge e i) => e -> EdgeSet e i -> EdgeSet e i
esAddEdge e (ES min mout)
  = let
    (vsrc, vdst) = edgeLimits e
  in ES (add min vdst e) (add mout vsrc e)
  where
    add m v e = M.alter (Just . maybe (S.singleton e) (S.insert e)) v m

-- |Removes an edge from the edge set.    
esRmvEdge :: (IsEdge e i) => e -> EdgeSet e i -> EdgeSet e i
esRmvEdge e (ES min mout)
  = let
    (vsrc, vdst) = edgeLimits e
  in ES (rm min vdst e) (rm mout vsrc e)
  where
    rm m v e = M.alter (const Nothing) v m
    
-- |Adds a node to the current edge set
esAddNode :: (IsEdge e i) => i -> EdgeSet e i -> EdgeSet e i
esAddNode v (ES mi mo) = ES (M.insert v S.empty mi) (M.insert v S.empty mo)

-- |Removes a given node and all it's outgoing and incomming edges.
esRmvNode :: (IsEdge e i) => i -> EdgeSet e i -> EdgeSet e i
esRmvNode v es@(ES min mout)
  = let
    eins = map edgeSrc . S.toList $ esGetIns  v es
    eous = map edgeDst . S.toList $ esGetOuts v es
    targets = eins ++ eous
  in ES (rm min v targets) (rm mout v targets)
  where
    rm :: (IsEdge e i) => M.Map i (S.Set e) -> i -> [i] -> M.Map i (S.Set e)
    rm m v eset = M.delete v $ foldr (rm1 v) m eset

    rm1 :: (IsEdge e i) => i -> i -> M.Map i (S.Set e) -> M.Map i (S.Set e)
    rm1 vi v = M.alter (maybe Nothing (Just . S.filter (not . edgeTouches vi))) v
    
infixr 9 ++> 
(++>) :: (IsEdge e i) => e -> EdgeSet e i -> EdgeSet e i
e ++> es = esAddEdge e es

--- *** Getters

-- |Returns the node indexes registered in the given edge set.
esNodes :: (IsEdge e i) => EdgeSet e i -> [i]
esNodes (ES mi mo) = M.keys mi `L.union` M.keys mo

-- |Returns the number of nodes in a graph
esSize :: (IsEdge e i) => EdgeSet e i -> Int
esSize = length . esNodes
    
-- |Get's the incomming arcs of a node
esGetIns :: (IsEdge e i) => i -> EdgeSet e i -> S.Set e
esGetIns v = maybe S.empty id . M.lookup v . edgesIn

-- |Return the indegree of a node.
esGetInDeg :: (IsEdge e i) => i -> EdgeSet e i -> Int
esGetInDeg v = S.size . esGetIns v

-- |We can also retrieve a given node parents
esNodeRho :: (IsEdge e i) => i -> EdgeSet e i -> S.Set i
esNodeRho v = S.map edgeSrc . esGetIns v

-- |Transitive closure of 'esNodeRho'
esNodeRhoStar :: (IsEdge e i) => i -> EdgeSet e i -> S.Set i
esNodeRhoStar v es = star (flip esNodeRho es) v

-- |Get's the outgoing arcs of a node
esGetOuts :: (IsEdge e i) => i -> EdgeSet e i -> S.Set e
esGetOuts v = maybe S.empty id . M.lookup v . edgesOut

-- |Return the outdegree of a node.
esGetOutDeg :: (IsEdge e i) => i -> EdgeSet e i -> Int
esGetOutDeg v = S.size . esGetOuts v

-- |Or, similarly, a node children
esNodeSigma :: (IsEdge e i) => i -> EdgeSet e i -> S.Set i
esNodeSigma v = S.map edgeDst . esGetOuts v

-- |Transitive closure of 'esNodeRho'
esNodeSigmaStar :: (IsEdge e i) => i -> EdgeSet e i -> S.Set i
esNodeSigmaStar v es = star (flip esNodeSigma es) v

-- |Returns the degree of a node.
esGetDegree :: (IsEdge e i) => i -> EdgeSet e i -> Int
esGetDegree v = uncurry (+) . split (esGetInDeg v) (esGetOutDeg v)

--------------------------------------------------------------------------------
-- * General Uility

-- |Transitive closure of f
star :: (Ord a) => (a -> S.Set a) -> a -> S.Set a 
star f i = staraux (S.singleton i) S.empty (f i)
  where
    staraux done aux vs
      = let ts = vs S.\\ done
            r' = S.foldr (\h -> S.union (f h)) vs ts
        in if S.null ts
           then aux
           else staraux (S.union done ts) (S.union r' aux) r'
           
--------------------------------------------------------------------------------
-- * Loop Cutset calculation
{-

Let G be a graph, we'll compute one loop cutset Cs using the following
heuristic:

Proc Loop-cutset(G, Cs):
  while there are nodes in G do:
    if vi \in G && degree(vi) \leq 1
    then select vi
    else get all nodes with indegree <= 1 (let these be the candidates)
         select a vi from the candidates with the minimal degree.
         add vi to Cs
    Delete vi
    
-}

-- |Returns the loop cutset of a graph
esGetCutset :: (IsEdge e i) => EdgeSet e i -> [i]
esGetCutset es = cutset es []
  where
    cutset es c
      | esSize es == 0 = c
      | otherwise
        = let
          ns = esNodes es
          (dels, ns') = L.partition ((<= 1) . flip esGetDegree es) ns
        in case dels of
            -- no dangling nodes, let's choose a node with minimal degree.
            [] -> let
                    (cs, _) = L.partition ((<= 1) . flip esGetInDeg es) ns
                    t       = head $ reverse $ L.sortBy (compare `on` (flip esGetDegree es)) cs
                  in cutset (esRmvNode t es) (t:c)
            
            -- dangling nodes! Just remove them and iterate.        
            _  -> cutset (foldr esRmvNode es dels) c

-- |Symbol ring graph    
lcs_t1 :: EdgeSet (Int, Int) Int
lcs_t1 = (1, 2)
     ++> (1, 3)
     ++> (2, 4)
     ++> (3, 4)
     ++> esEmpty
            
-- |Slide 204
lcs_t2 :: EdgeSet (Int, Int) Int
lcs_t2 = (1, 3)
     ++> (3, 2)
     ++> (3, 4)
     ++> (5, 4)
     ++> (5, 6)
     ++> (4, 7)
     ++> (6, 7)
     ++> (7, 8)
     ++> (7, 10)
     ++> (7, 9)
     ++> (8, 10)
     ++> (9, 10)
     ++> (9, 11)
     ++> esEmpty
     
-- |Pretzel
lcs_t3 :: EdgeSet (Int, Int) Int
lcs_t3 = (5, 2)
     ++> (5, 6)
     ++> (2, 7)
     ++> (6, 7)
     ++> lcs_t1
     
-- |Loopless big graph
lcs_t4 :: EdgeSet (Int, Int) Int
lcs_t4 = foldr (++>) esEmpty [ (i-1, i) | i <- [2..50] ]

--------------------------------------------------------------------------------
-- * QuickCheck Properties

-- |Given a function over a node and a edge set, verify the invariant in the
--  given edge set.
propNodeInv :: (IsEdge e i) => (i -> Bool) -> EdgeSet e i -> Bool
propNodeInv inv es = all inv $ esNodes es

-- |All predicate, extended to Sets
sall :: (a -> Bool) -> S.Set a -> Bool
sall f = all f . S.toList

-- |Every Node is a parent of it's childs.
propSigmaCorrect :: EdgeSet (Int, Int) Int -> Bool
propSigmaCorrect es 
  = propNodeInv (\n -> sall (\m -> (n `S.member` esNodeRho m es)) $ esNodeSigma n es) es

-- |Every Node is a child of it's parents.
propRhoCorrect :: EdgeSet (Int, Int) Int -> Bool
propRhoCorrect es
  = propNodeInv (\n -> sall (\m -> (n `S.member` esNodeSigma m es)) $ esNodeRho n es) es


