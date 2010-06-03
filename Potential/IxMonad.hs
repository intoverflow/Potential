{-# LANGUAGE
	NoImplicitPrelude,
	UndecidableInstances,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Potential.IxMonad
	( IxMonad(..), IxMonadTrans(..), Composable, Terminal
	) where

import Prelude( ($) )
import Data.Monoid

import Potential.IxMonad.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.Reader
import Potential.IxMonad.Region
import Potential.IxMonad.State
import Potential.IxMonad.Writer

-- Lift IxStateT
instance (IxMonadReader r m) => IxMonadReader r (IxStateT s m) where
  ask = lift ask

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxStateT s m) where
  tell w = lift $ tell w

-- Lift IxReaderT
instance (IxMonadState s m) => IxMonadState s (IxReaderT r m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxReaderT r m) where
  tell w = lift $ tell w

-- Lift IxWriterT
instance (Monoid w, IxMonadState s m) => IxMonadState s (IxWriterT w m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadReader r m) => IxMonadReader r (IxWriterT w m) where
  ask = lift ask


-- Lift IxConstrainedT
instance (IxMonadState s m) => IxMonadState s (IxConstrainedT c m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxConstrainedT c m) where
  tell w = lift $ tell w

instance (IxMonadReader r m) => IxMonadReader r (IxConstrainedT c m) where
  ask = lift ask


-- Lift IxRegionT
instance (IxMonadState s m) => IxMonadState s (IxRegionT typ r m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxRegionT typ r m) where
  tell w = lift $ tell w

instance (IxMonadReader r m) => IxMonadReader r (IxRegionT typ s m) where
  ask = lift ask


