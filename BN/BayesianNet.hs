{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|BayesianNet

This module provides a very naive implementation of Pearl's Inference
algorithm on Bayesian Networks.

It is similar to SamIan in which query and edit mode are different.
Inside the BN monad you may edit your network and you should use
QueryBN to ask probabilities. It's easy to convert a QueryBN back to a BN
with 'bnQuery' function.

>
> f <- bnQuery $ bnDataFusion "mynode"
>

Will return the data-fusion lemma for node "mynode". Note that bnDataFusion returns a QueryBN.

Without going too technical, the main difference about BN and QueryBN is a memoization layer.
In fact, 'QueryBN' = 'BN' + memoization. This explicit conversion between the two forces the
recomputation of parameters to happen. Note that, since 'QueryBN' is also a monad, you can
(and should) setup your network and then run all the queries you want.

There are two CPP flags that can be set to slightly change the behaviour:

  1. @MSG_DOMAIN_CHECK@
      Will add a domain check to the parameters we compute, allowing us to produce
      nicer error messages. Should only be needed if you are messing around
      with pi's and lambda's manually (which shouldn't be needed)
      
  2. @MSG_TRACE@
      Will display a trace of the messages that were exchanged in the network.
      Won't display values though, since we compute the messages as functions
      (just like their formal definition).
-}
module BN.BayesianNet(
  -- * Type Synonyms
    Prob, Val, Lbl, Type, GammaTable
  
  -- * Monads
  , BN, QueryBN
  
  -- * Data
  , CompiledBN
  
  -- * Functions
  -- ** Unwrapping
  , runBN   
  , bnQuery
  
  -- ** Edit
  , gammaAddNames
  , bnGammaLkup
  , bnAddNode
  , bnAddArc
  , bnSetGamma
  , bnCompile
  
  -- ** Observation
  , bnObserve  
  , bnClear              
  , bnGetObs
  
  -- ** Querying
  , bnDataFusion
  )
  where

-- #define MSG_DOMAIN_CHECK 1
-- #define MSG_TRACE        1

import Prelude hiding (pi)

import BN.Common
import BN.Types
import BN.EdgeSet

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List((\\), sortBy)
import Data.Function(on)

import Control.Monad.Memo

#ifdef MSG_TRACE
import qualified Debug.Trace as T
#endif

-----------------------------------------------------------------------------------------------
-- * Important Types and Internal Reps.

-- ** Synonyms

type Prob = Float

type Type = String
type Val  = String
type Lbl  = String

type GammaTable = M.Map (Val, [(Lbl, Val)]) Prob

-- ** Nodes

-- |Bayesian net node information.
data BNode
  -- |A node can be a normal variable,
  = BNodeVar 
      { bnodeVarLbl   :: Lbl
      , bnodeVarType  :: Type
      , bnodeVarGamma :: GammaTable
      }
  -- |Or a dummy, representing an observation for a variable.
  | BNodeDummy
      { bnodeDummyFor :: Lbl
      , bnodeDummyVal :: Val
      }
      
-- *** Node Utils
      
-- |Returns the label for a given node.
bnodeLbl :: BNode -> Lbl
bnodeLbl (BNodeDummy l _) = bnodeDummyName l
bnodeLbl n                = bnodeVarLbl n

bnodeDummyName :: Lbl -> Lbl
bnodeDummyName l = '_':'_':l
  
-- ** The Network

-- |Imaps
type IMap = EdgeSet (Lbl, Lbl) Lbl

-- |Network specific data
data BayesianNetData = BND
  { bnnodes :: M.Map Lbl BNode
  , bngraph :: IMap
  , bnobs   :: [Lbl]
  }
  
-- |Loop cutset data
type LCSData = M.Map Lbl (Val -> Prob)
  
-- |State Monadic wrapper

-- |Represents a compiled network.  
data CompiledBN
  = CBN BayesianNetData TyDict

-- |Encapsulates a network inside state-monads to make life easier.  
type BN m = StateT BayesianNetData (Err (Ty m))

-- |Syntax-sugar instance; it just means m is a Functor, a Monad and Applicative.
--  Check 'BN.Common'.
instance M m => M (BN m) where

-- ** Unwrapping

-- |Runs a Bayesian-network computation
runBN :: (M m) => [(Type, [Val])] -> BN m a -> m (Either BError a)
runBN tys = runTyped tys . runExceptT . flip evalStateT (BND M.empty esEmpty [])

runBN' :: (M m) => TyDict -> BN m a -> m (Either BError a)
runBN' tdict = runTypedFor tdict . runExceptT . flip evalStateT (BND M.empty esEmpty [])
  
-- *** Network private interface

-- |Returns the BNode with label @lbl@, throws an error if the node is not registered.
__bnGetNode :: (M m) => Lbl -> BN m BNode
__bnGetNode l = (M.lookup l . bnnodes <$> get) 
            >>= maybe (errLoc "BN.BayesianNet.__bnGetNode" $  "No such node: " ++ l)
                      return
                      
-- |Returns the type of a given node
__bnGetNodeType :: (M m) => Lbl -> BN m Type
__bnGetNodeType l
  = do
    n <- __bnGetNode l
    case n of
      (BNodeDummy {}) -> __bnGetNodeType (bnodeDummyFor n)
      (BNodeVar{})    -> return (bnodeVarType n)

-- |Runs a function over our graph
__bnGraphF :: (M m) => (IMap -> a) -> BN m a
__bnGraphF f = f . bngraph <$> get

-- |Runs a function over a given node
__bnNodeF :: (M m) => (BNode -> a) -> Lbl -> BN m a
__bnNodeF f n = __bnGetNode n >>= return . f

-- |Runs a type-level function for a given node.
__bnTyF :: (M m) => (Type -> Ty m a) -> Lbl -> BN m a
__bnTyF f n = __bnGetNodeType n >>= __bnTyL . f

-- |Lifts a type computation to BN.
__bnTyL :: (M m) => Ty m a -> BN m a
__bnTyL = lift . lift

-- |Sorts a given list of label-value pairs.
__bnSortPs :: [(Lbl, Val)] -> [(Lbl, Val)]
__bnSortPs = sortBy (compare `on` fst)

-----------------------------------------------------------------------------------------------
-- * Publics

-- ** Primitives

-- |Returns @x \rightarrow \gamma(x | ps)@ for @x@ beeing a possible value for
--  n.
bnGammaLkup :: (M m) => Lbl -> [(Lbl, Val)] -> BN m (Val -> Prob)
bnGammaLkup n ps = __bnNodeF (lkup (__bnSortPs ps)) n
  where
    -- TODO: add a safer version of M.!, we have a MonadError afterall.
    lkup :: [(Lbl, Val)] -> BNode -> Val -> Prob
    lkup ps bn@(BNodeVar {}) = \s -> (bnodeVarGamma bn) M.! (s, ps) 
    lkup ps bn               = error $ "Can't get gamma for dummy of " ++ bnodeDummyFor bn

-- |Given an assessment table, adds the correct labels. 
gammaAddNames :: [Lbl] -> [((Val, [Val]), Prob)] -> GammaTable
gammaAddNames ps = M.fromList . map ((id >< (zip ps) >< id)) 

-- ** Construction

-- |Adds a node to the network. The type gotta be registered previously, otherwise
--  and exception is thrown.
bnAddNode :: (M m) => Lbl -> Type -> BN m ()
bnAddNode n ty
  = do
    -- assert that we know about this type.
    assertM_ (__bnTyL $ tyTyIsDecl ty)
    __bnTyL (tyDeclVar n ty)
    modify (\s -> s { bnnodes = M.insert n (BNodeVar n ty M.empty) (bnnodes s) } )

-- |Adds an arc between two existing nodes.
bnAddArc :: (M m) => Lbl -> Lbl -> BN m ()
bnAddArc n m
  = do
    -- assert that both nodes exists.
    mapM_ __bnGetNode [n, m]
    modify (\s -> s { bngraph = esAddEdge (n, m) (bngraph s) })

-- |Sets the assessment function for a given node.
--  Will perform both a length check and a semantic check.
bnSetGamma :: (M m) => Lbl -> GammaTable -> BN m ()
bnSetGamma n t
  = do
    n'   <- __bnGetNode n
    -- who are n's parents?
    nrho <- S.toList <$> __bnGraphF (esNodeRho n)
    
    -- how many possiblities for each parent and n' itself?
    nrhoTys <- mapM (__bnTyL . tyValsFor) $ n:nrho
    nrhoTysCard <- 
        maybe (errLoc "BN.BayesianNet.bnSetGamma:" $ "Unkown type for some of: " ++ unwords (n:nrho))
          (return . product . map length)
          (sequence nrhoTys)
    
    -- do we have the correct number of parameters?
    when (M.size t /= nrhoTysCard)
      $ errLoc ("BN.BayesianNet.bnSetGamma " ++ n ++ ":") $ "Expected " ++ show nrhoTysCard ++ " parameters."
      
    modify (\s -> s { bnnodes = M.alter (maybe Nothing (\n -> Just n{ bnodeVarGamma = t })) n (bnnodes s) })
    
    -- is this table correct? do the proper parameters sum to 1, as they're supposed to?
    (Just nvals) <- __bnTyL (tyVals (bnodeVarType n'))
    (Just crho)  <- __bnTyL (tyConfs nrho)
    mapM_ (validateConf n nvals) crho
  where
    validateConf :: (M m) => Lbl -> [Val] -> [(Lbl, Val)] -> BN m ()
    validateConf n nvals cr
      = do
        -- T.trace (show cr) (return ())
        gamma <- bnGammaLkup n cr
        when (sum (map gamma nvals) /= 1.0)
          $ errLoc ("BN.BayesianNet.bnSetGamma " ++ n ++ ":") $ "Configuration " ++ show cr ++ " does not sum 1."
        

-- ** Observation

-- |Adds an observation for a variable. Throws exceptions if the
--  value if invalid or if the variable does not exist.
bnObserve :: (M m) => Lbl -> Val -> BN m ()
bnObserve lbl val
  = do
    assertM_ (__bnTyF (tyValidVal val) lbl)
    let n  = BNodeDummy lbl val
    let nl = bnodeLbl n
    ty <- bnodeVarType <$> __bnGetNode lbl
    __bnTyL (tyDeclVar nl ty)
    modify (\s -> s { bngraph = esAddEdge (lbl, nl) (esAddNode nl (bngraph s))
                    , bnnodes = M.insert nl n (bnnodes s)
                    , bnobs   = lbl : bnobs s })

-- |Clear an observation made for a node. Does not raise an
--  exception if the not was uninstantiated.    
bnClear :: (M m) => Lbl -> BN m () 
bnClear lbl
  = do
    -- we can only remove an observed node.
    hasobs <- (lbl `elem`) . bnobs <$> get
    when hasobs
      $ do
        __bnTyL (tyUndeclVar $ bnodeDummyName lbl)
        modify (\s -> s { bngraph = esRmvNode (bnodeDummyName lbl) (bngraph s)
                        , bnnodes = M.delete (bnodeDummyName lbl) (bnnodes s)
                        , bnobs   = (bnobs s) \\ [lbl]
                        })
      
-- |Returns the observed value for a node, if this node is observed.                  
bnGetObs :: (M m) => Lbl -> BN m (Maybe Val)
bnGetObs lbl
  = do
    s <- get
    if (lbl `elem` (bnobs s))
      then maybe (panic "bnGetObs")
                 (return . Just . bnodeDummyVal) 
                 $ M.lookup (bnodeDummyName lbl) (bnnodes s)
      else return Nothing
      
-- |Returns the current network data.
bnCompile :: (M m) => BN m CompiledBN
bnCompile
  = do
    net <- get
    tys <- lift $ lift get
    return (CBN net tys)
      
-----------------------------------------------------------------------------------------------
-- * Loop Cutset

-- TODO: IMPLEMENT!!!

-----------------------------------------------------------------------------------------------
-- * Recursive Conditioning

bnRecursiveCond :: (M m) => Lbl -> BN m (Val -> Prob)
bnRecursiveCond v
  = do
    ps  <- S.toList              <$> __bnGraphF (esNodeRho v)
    fps <- (M.fromList . zip ps) <$> mapM bnRecursiveCond ps
    cps <- __bnTyL (tyConfs ps) >>= maybe (panic "bnRecursiveCond") return
    pv  <- mapM (marginalize fps) cps
    return $ summ pv
  where
    marginalize fps c
      = do
        g <- bnGammaLkup v c
        let r = foldr (\(plbl, val) r -> (fps M.! plbl) val * r) 1 c
        return $ \b -> r * g b
    
    

-----------------------------------------------------------------------------------------------
-- * Pearl's Algorithm Specifics (lowlevel)
--
-- This is a very low level and will trigger a lot of repeated computations.
-- These functions are used as building blocks for the higher level 'BayesT'.

instance M m => M (MemoT i v m) where

-- |Adding memoization;
type QueryBN m = MemoT Parm [(Val, Prob)] (BN m)

-- |Unwraps a QueryBN computation
bnQuery :: (M m) => QueryBN m a -> BN m a
bnQuery = startEvalMemoT

-- |Runs a query over a non-monadic network.
bnQueryOver :: (M m) => CompiledBN -> QueryBN m a -> m (Either BError a)
bnQueryOver (CBN bnet tys) q = runBN' tys (put bnet >> bnQuery q)

-- |Parameters
data Parm
  = PLam  Lbl Lbl
  | PLamC Lbl
  | PPi   Lbl Lbl
  | PPiC  Lbl
  | PNul
  deriving (Eq, Show, Ord)

-- |Wrapping of parameters under the memoization interface
pi, lam :: (M m) => Lbl -> Lbl -> QueryBN m (Val -> Prob)
pi  vi vo = memoParm (PPi vi vo)
lam vi vo = memoParm (PLam vi vo)

-- |Wrapping of compound parameters under the memoization interface
piC, lamC :: (M m) => Lbl -> QueryBN m (Val -> Prob)
piC  v = memoParm (PPiC v)
lamC v = memoParm (PLamC v)

-- ** Internal utilities

prod :: [Val -> Prob] -> Val -> Prob
prod fs b = foldr (\h r -> h b * r) 1 fs

summ :: [Val -> Prob] -> Val -> Prob
summ fs b = foldr (\h r -> h b + r) 0 fs

dirac :: Lbl -> Val -> [Val] -> Val -> Prob
dirac n v vs i
  | i `elem` vs = if i == v then 1.0 else 0.0
  | otherwise   = error $ i ++ " should be an element of {" ++ unwords vs ++ "}"

normalize :: (M m) => Type -> (Val -> Prob) -> BN m (Val -> Prob)
normalize ty f
  = do
    nvals <- __bnTyL (tyVals ty) >>= maybe (panic "normalize") return
    let alpha = 1.0 / sum (map f nvals)
    return (\v -> alpha * f v)
    
-----------------------------------------------------------------------------------------------
-- * Pearl's Algorithm Low Level Interface
    
-- |Computes the causal parameter @pi vi vo@ that @vo@ receives from @vi@.
pPi :: (M m) => Lbl -> Lbl -> QueryBN m (Val -> Prob)
pPi vi vo = lift (bnGetObs vi) >>= maybe (piNoInst vi vo) (piInst vi vo)
  where
-- MSG_DOMAIN_CHECK will add a domain-check in messages transmitted from one
-- node to another. Might me usefull if the user is going to use this messages by
-- hand.
#ifndef MSG_DOMAIN_CHECK
    piInst vi vo b
      = return (\b' -> if b == b' then 1.0 else 0.0)
#else
    piInst vi vo b
      = do
        domain <- lift $ __bnTyL (tyValsFor vi) >>= maybe (panic "piInst") return
        return (\b' -> if b' `elem` domain
                       then if b == b' then 1.0 else 0.0
                       else error $ b' ++ " does not belong to {" ++ unwords domain 
                                       ++ "}, in pPi " ++ vi ++ " " ++ vo)
#endif

    piNoInst vi vo
      = do
        vic      <- piC vi
        vitype   <- lift $ bnodeVarType <$> __bnGetNode vi
        children <- lift $ S.toList     <$> __bnGraphF (esNodeSigma vi)
        lams     <- mapM (lam vi) children
        normalize vitype (prod $ vic : lams)


-- |Computes the compound causal parameter for node @vi@.
pPiC :: (M m) => Lbl -> QueryBN m (Val -> Prob)
pPiC vi
  = do
    -- get every parent
    parents <- lift $ S.toList <$> __bnGraphF (esNodeRho vi)
    
    -- "receive" the causal parameters from each parent.
    sppis   <- mapM (\p -> pi p vi) parents
    
    -- computes every possible configuration for my parents.
    pconfs  <- lift $ __bnTyL (tyConfs parents) 
               >>= maybe (panic "BayesianNet.hs:233") return
               
    summ <$> mapM (mkFactor sppis) pconfs
  where
    -- mks a factor. I'm assuming that conf and sppi are in the same order.
    mkFactor :: (M m) => [Val -> Prob] -> [(Lbl, Val)] -> QueryBN m (Val -> Prob)
    mkFactor sppis conf
      = do
        g <- lift $ bnGammaLkup vi conf 
        let r = product $ map (\(pi, c) -> pi c) (zip sppis (map snd conf))
        return (\b -> g b * r) 
    
     
-- |Computes the diagnostic parameter for a given node
pLam :: (M m) => Lbl -> Lbl -> QueryBN m (Val -> Prob)
pLam vi vo 
  = do
    obs <- lift $ bnobs <$> get
    if obs == []
      then return $ const 1.0 -- if there are no observations, all diagnostic parameters are 1.0.
      else lamObs vi vo
  where
    lamObs vi ovo@('_':'_':vo)
      = do
        vovals <- lift $ __bnTyL (tyValsFor vo) >>= maybe (panic "lamObs, __, tyValsFor") return
        obsval <- lift $ bnodeDummyVal <$> __bnGetNode ovo
        return (dirac vo obsval vovals)
    lamObs vi vo
      = do
        rho'  <- lift $ S.toList . (S.\\ (S.singleton vi)) <$> __bnGraphF (esNodeRho vo)
        crho' <- lift $ __bnTyL (tyConfs rho') >>= maybe (panic "lamObs, tyConfs") return
        vocs  <- lift $ __bnTyL (tyValsFor vo) >>= maybe (panic "lamObs, tyValsFor") return
        summ <$> mapM (mkOuterFactor crho' vi vo) vocs
        
    mkOuterFactor :: (M m) => [[(Lbl, Val)]] -> Lbl -> Lbl -> Val -> QueryBN m (Val -> Prob)
    mkOuterFactor crho' vi vo voc
      = do
        l     <- lamC vo 
        let inner = map (mkInnerFactor vi vo voc) crho' -- :: [Val -> QueryBN m Prob]
        
        -- now, we preconpute the inner possible 
        -- values and build a suitably typed Haskell function.
        vivals <- lift $ __bnTyL (tyValsFor vi) >>= maybe (panic "mkOuterFactor") return
        vs     <- map buildFunction <$> mapM (flip precompute vivals) inner
        
        return (\vic -> l voc * (summ vs) vic)
        
    precompute :: (M m) => (Val -> QueryBN m Prob) -> [Val] -> QueryBN m [(Val, Prob)]
    precompute innerf vals = mapM (\v -> innerf v >>= return . (v,)) vals
    
    buildFunction :: (Ord a) => [(a, b)] -> a -> b
    buildFunction dict a = (M.fromList dict) M.! a
     
    mkInnerFactor :: (M m) => Lbl -> Lbl -> Val -> [(Lbl, Val)] -> Val -> QueryBN m Prob   
    mkInnerFactor vi vo voc crho vic
      = do
        beta <- product <$> mapM (\(b, bv) -> pi b vo >>= return . ($ bv)) crho
        g <- lift $ bnGammaLkup vo ((vi, vic) : crho) 
        return (g voc * beta)

-- |Computes the compound diagnostic parameter for a given node.
pLamC :: (M m) => Lbl -> QueryBN m (Val -> Prob)
pLamC v = lift (__bnGraphF $ esNodeSigma v) >>= mapM (lam v) . S.toList >>= return . prod

-----------------------------------------------------------------------------------------------
-- * Pearl's Algorithm Interface
--
-- This interface adds memoization to the stir.

{-

-- TODO: template for using ArrayCache's as memoization framework.
-- TODO: use a monad writter to trace messages?

indexParm :: (M m) => Parm -> BN m Int
indexParm = undefined

unindexParm :: (M m) => Int -> BN m Parm
unindexParm = undefined
    
-- |Memoization wrapper for messages.
memoParm :: (M m) => Parm -> BN (MemoT Int [(Val, Prob)] m) (Val -> Prob)
memoParm p = indexParm p >>= memol3 runMsgIndexed >>= return . untabulate
  where
    runMsgIndexed p = unindexParm p >>= runMsg
  
    runMsg (PLam vi vo) = pLam vi vo >>= tabulate vi
    runMsg (PLamC v)    = pLamC v    >>= tabulate v
    runMsg (PPi vi vo)  = pPi vi vo  >>= tabulate vi
    runMsg (PPiC v)     = pPiC v     >>= tabulate v
    
    untabulate :: [(Val, Prob)] -> Val -> Prob
    untabulate tab v
      = (M.fromList tab) M.! v
-}

-- |Memoization wrapper for messages.
memoParm :: (M m) => Parm -> BayesT m (Val -> Prob)
memoParm p = memol3 runMsg' p >>= return . untabulate
  where 
    runMsg' p = traceMe p >> runMsg p
  
#ifdef MSG_TRACE
    traceMe p = T.trace (show p) (return ())
#else
    traceMe _ = return ()
#endif

    runMsg (PLam vi vo) = pLam vi vo >>= tabulate vi
    runMsg (PLamC v)    = pLamC v    >>= tabulate v
    runMsg (PPi vi vo)  = pPi vi vo  >>= tabulate vi
    runMsg (PPiC v)     = pPiC v     >>= tabulate v
    
    untabulate :: [(Val, Prob)] -> Val -> Prob
    untabulate tab v
      = (M.fromList tab) M.! v
      
tabulate :: (M m) => Lbl -> (Val -> Prob) -> BN m [(Val, Prob)]
tabulate l f
  = do
    tys <- __bnTyL (tyValsFor l) >>= maybe (panic "tabulate") return
    return [ (i, f i) | i <- tys ]

-- ** Data Fusion Lemma

bnDataFusion :: (M m) => Lbl -> BayesT m (Val -> Prob)
bnDataFusion lbl
  = do
    ty <- bnodeVarType <$> __bnGetNode lbl
    l  <- lamC lbl
    p  <- piC  lbl
    normalize ty (prod [p, l])
     
     
        
