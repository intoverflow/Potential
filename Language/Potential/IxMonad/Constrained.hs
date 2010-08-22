{-# LANGUAGE
	NoImplicitPrelude,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Language.Potential.IxMonad.Constrained
	( IxConstrainedT(..)
	) where

import Prelude( ($) )
import Language.Potential.IxMonad.IxMonad

newtype IxConstrainedT c m ct x y a =
    IxConstrainedT { runIxConstrainedT :: c -> m ct x y a }

instance IxMonadTrans (IxConstrainedT c) where
  lift op = IxConstrainedT $ \c -> op

instance IxFunctor m => IxFunctor (IxConstrainedT c m) where
  fmap f m = IxConstrainedT $ \c -> fmap f (runIxConstrainedT m c)

instance IxMonad m => IxMonad (IxConstrainedT c m) where
  unsafeReturn a = lift $ unsafeReturn a
  st >>= f = IxConstrainedT $ \c -> runIxConstrainedT st c >>= \a ->
				    let st' = f a
				    in runIxConstrainedT st' c

