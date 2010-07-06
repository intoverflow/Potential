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
  get :: m (Unmodeled x x) s
  put :: s -> m (Unmodeled x x) ()

newtype IxStateT s m ct a =
    IxStateT { runIxStateT :: s -> m ct (a, s) }

instance IxMonadTrans (IxStateT s) where
  lift op = IxStateT $ \s -> fmap (\a -> (a, s)) op

instance IxFunctor m => IxFunctor (IxStateT s m) where
  fmap f m = IxStateT $ \s -> fmap (\(a, s') -> (f a, s')) (runIxStateT m s)

instance IxMonad m => IxMonad (IxStateT s m) where
  unsafeReturn a = lift $ unsafeReturn a
  st >>= f = IxStateT $ \s -> runIxStateT st s >>= \(a, s') ->
			      let st' = f a
			      in runIxStateT st' s'

instance (IxMonad m) => IxMonadState s (IxStateT s m) where
  get   = IxStateT $ \s -> return (s, s)
  put s = IxStateT $ \_ -> return ((), s)

