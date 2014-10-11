{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module BN.Types where

import BN.Common

import qualified Data.Map as M
import Data.Maybe()

data TyDict = TyDict
  { tynames :: [String]
  , tyvals  :: M.Map String [String]
  , tyvars  :: M.Map String String
  } deriving Show

type Ty = StateT TyDict

instance (M m) => M (Ty m) where

-----------------------------------------------------------------------------------------------
-- * Unwrapping

runTyped :: (M m) => [(String, [String])] -> Ty m a -> m a
runTyped tys = let ns = map fst tys
               in assert (validTypes tys) $ flip evalStateT (TyDict ns (M.fromList tys) M.empty)
             where
               -- no empty types allowed.
               validTypes = all ((> 0) . length . snd)

-----------------------------------------------------------------------------------------------
-- * Interface

-----------------------------------------------------------------------------------------------
-- ** Predicates

-- |Is a type declared?
tyTyIsDecl :: (M m) => String -> Ty m Bool
tyTyIsDecl s = (s `elem`) . tynames <$> get

-- |Is a variable declared?
tyVarIsDecl :: (M m) => String -> Ty m Bool
tyVarIsDecl s = (s `elem`) . M.keys . tyvars <$> get

-----------------------------------------------------------------------------------------------
-- ** Functionality

-- |Declares a new variable @v@ with type @t@. If @t@ is not registered as a type
--  will throw an exception. Will not check for redeclarations, though.
tyDeclVar :: (M m) => String -> String -> Ty m ()
tyDeclVar var ty
  = do
    assertM_ ((&&) <$> tyTyIsDecl ty <*> (not <$> tyVarIsDecl var))
    modify (\s -> s { tyvars = M.insert var ty (tyvars s) })
    
-- |Deletes a variable
tyUndeclVar :: (M m) => String -> Ty m ()
tyUndeclVar var = modify (\s -> s { tyvars = M.delete var (tyvars s) })
    
-- |Returns the type of a variable, if it exists.
tyOf :: (M m) => String -> Ty m (Maybe String)
tyOf s = M.lookup s . tyvars <$> get

-- |Returns the values of a type, if it exists.
tyVals :: (M m) => String -> Ty m (Maybe [String])
tyVals s = M.lookup s . tyvals <$> get

tyValsFor :: (M m) => String -> Ty m (Maybe [String])
tyValsFor s = tyOf s >>= maybe (return Nothing) tyVals 

-- |Is a given value member of a given type?
tyValidVal :: (M m) => String -> String -> Ty m Bool
tyValidVal val ty = (val `elem`) . maybe [] id <$> tyVals ty

-- |Returns all possible configurations for a given set of variables.
tyConfs :: (M m) => [String] -> Ty m (Maybe [[(String, String)]])
tyConfs vs
  = do
    vsvals <- mapM tyValsFor vs
    return (sequence vsvals >>= return . combine vs)
  where
    combine :: [String] -> [[String]] -> [[(String, String)]]
    combine [] _ = [[]]
    combine (v:vs) (ty:tys)
      = let cvs = combine vs tys
        in [(v, t):c | t <- ty, c <- cvs ]
        







