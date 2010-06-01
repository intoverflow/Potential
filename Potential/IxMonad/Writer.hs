{-# LANGUAGE
	NoImplicitPrelude,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Potential.IxMonad.Writer
	( IxMonadWriter(..)
	, IxWriterT(..)
	) where

import Prelude( ($) )
import Data.Monoid

import Potential.IxMonad.IxMonad


class (Monoid w, IxMonad m) => IxMonadWriter w m | m -> w where
  tell   :: w -> m x x ct ()
  -- listen :: m x ct a -> m x ct (a, w)
  -- pass   :: m x ct (a, w -> w) -> m x ct a


newtype IxWriterT w m x y ct a =
    IxWriterT { runIxWriterT :: m x y ct (a, w) }

instance (Monoid w) => IxMonadTrans (IxWriterT w) where
  lift op = IxWriterT $ op >>>= \a -> return (a, mempty)

instance (Monoid w, IxMonad m) => IxMonad (IxWriterT w m) where
  mixedReturn a = lift $ mixedReturn a
  wr >>>= f = IxWriterT $ runIxWriterT wr >>>= \(a, w) ->
			  let wr' = f a
			  in runIxWriterT wr' >>>= \(b, w') ->
			  return (b, w `mappend` w')

instance (Monoid w, IxMonad m) => IxMonadWriter w (IxWriterT w m) where
  tell w = IxWriterT $ return ( (), w )


