{-# LANGUAGE
	NoImplicitPrelude,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Potential.IxMonad.PState
	( IxPState(..)
	) where

import Prelude( ($) )
import Potential.IxMonad.IxMonad

class (IxMonad m) => IxPState m where
  psGet :: m Unmodeled x x x
  psPut :: y -> m Composable x y ()
  psModify :: (x -> y) -> m (Compose (Compose Unmodeled Unmodeled) Composable) x y ()
  psModify f = do a <- psGet
		  psPut (f a)

