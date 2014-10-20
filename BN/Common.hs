{-# LANGUAGE FlexibleContexts #-}
module BN.Common(
    module Control.Monad
  , module Control.Applicative
  , module Control.Monad.State
  , module Control.Monad.Except
  , module Control.Monad.Identity
  , BError
  , split
  , (><)
  , (-|-)
  , M(..)
  , Err
  , panic
  , err
  , errLoc
  , assert
  , assertM
  , assertM_
  ) where

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Exception(assert)

-----------------------------------------------------------------------------------------------
-- * Usefull stuff

i1 = Left

i2 = Right

p1 = fst

p2 = snd

split f g a = (f a, g a)

f >< g = split (f . fst) (g . snd)

f -|- g = either (f . i1) (g . i2)

-----------------------------------------------------------------------------------------------
-- * Type-class wrappings

class (Functor m, Monad m, Applicative m)
      => M m where
      
instance M Identity where

instance M IO where

-----------------------------------------------------------------------------------------------
-- * Synonyms

type Err m = ExceptT BError m

data BError
  = PANIC String
  | BErrStr String
  | BErrStrLoc String String
  deriving Show
  
panic, err :: (MonadError BError m) => String -> m a
panic      = throwError . PANIC
err        = throwError . BErrStr

errLoc :: (MonadError BError m) => String -> String -> m a
errLoc s l = throwError $ BErrStrLoc s l

assertM :: (M m) => m Bool -> m a -> m a
{-# INLINE assertM #-}
assertM pred val
  = do
    b <- pred
    assert b val
    
assertM_ :: (M m) => m Bool -> m ()
{-# INLINE assertM_ #-}
assertM_ pred = assertM pred (return ())
