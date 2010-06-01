{-# LANGUAGE
	NoImplicitPrelude,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Potential.IxMonad.State
	( IxMonadState(..)
	, IxStateT(..)
	) where

import Prelude( ($) )
import Potential.IxMonad.IxMonad

class (IxMonad m) => IxMonadState s m | m -> s where
  get :: m x x ct s
  put :: s -> m x x ct ()

newtype IxStateT s m x y ct a =
    IxStateT { runIxStateT :: s -> m x y ct (a, s) }

instance IxMonadTrans (IxStateT s) where
  lift op = IxStateT $ \s -> op >>>= \a -> return (a, s)

instance IxMonad m => IxMonad (IxStateT s m) where
  mixedReturn a = lift $ mixedReturn a
  st >>>= f = IxStateT $ \s -> runIxStateT st s >>>= \(a, s') ->
			       let st' = f a
			       in runIxStateT st' s'

instance (IxMonad m) => IxMonadState s (IxStateT s m) where
  get   = IxStateT $ \s -> return (s, s)
  put s = IxStateT $ \_ -> return ((), s)

