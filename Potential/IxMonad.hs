{-# LANGUAGE
	NoImplicitPrelude,
	UndecidableInstances,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Potential.IxMonad
	( Composition(..)
	, IxFunctor(..), IxMonad(..), IxMonadTrans(..)
	, Unmodeled, Composable, Terminal
	, unmodeled, composable, terminal
	) where

import Prelude( ($) )
import Data.Monoid

import Potential.IxMonad.IxMonad
import Potential.IxMonad.Constrained
import Potential.IxMonad.Reader
import Potential.IxMonad.Region
import Potential.IxMonad.State
import Potential.IxMonad.Writer
import Potential.IxMonad.PState

-- Lift IxStateT
instance (IxMonadReader r m) => IxMonadReader r (IxStateT s m) where
  ask = lift ask

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxStateT s m) where
  tell w = lift $ tell w

instance IxPState m => IxPState (IxStateT s m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

instance IxMonadRegion m => IxMonadRegion (IxStateT s m) where
  type RegionType (IxStateT s m) = RegionType m
  type RegionLabel (IxStateT s m) = RegionLabel m

-- Lift IxReaderT
instance (IxMonadState s m) => IxMonadState s (IxReaderT r m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxReaderT r m) where
  tell w = lift $ tell w

instance IxPState m => IxPState (IxReaderT r m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

instance IxMonadRegion m => IxMonadRegion (IxReaderT r m) where
  type RegionType (IxReaderT r m) = RegionType m
  type RegionLabel (IxReaderT r m) = RegionLabel m

-- Lift IxWriterT
instance (Monoid w, IxMonadState s m) => IxMonadState s (IxWriterT w m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadReader r m) => IxMonadReader r (IxWriterT w m) where
  ask = lift ask

instance (Monoid w, IxPState m) => IxPState (IxWriterT w m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

instance (Monoid w, IxMonadRegion m) => IxMonadRegion (IxWriterT w m) where
  type RegionType (IxWriterT w m) = RegionType m
  type RegionLabel (IxWriterT w m) = RegionLabel m

-- Lift IxConstrainedT
instance (IxMonadState s m) => IxMonadState s (IxConstrainedT c m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxConstrainedT c m) where
  tell w = lift $ tell w

instance (IxMonadReader r m) => IxMonadReader r (IxConstrainedT c m) where
  ask = lift ask

instance IxPState m => IxPState (IxConstrainedT c m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

instance IxMonadRegion m => IxMonadRegion (IxConstrainedT c m) where
  type RegionType (IxConstrainedT c m) = RegionType m
  type RegionLabel (IxConstrainedT c m) = RegionLabel m

-- Lift IxRegionT
instance (IxMonadState s m) => IxMonadState s (IxRegionT typ r m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxRegionT typ r m) where
  tell w = lift $ tell w

instance (IxMonadReader r m) => IxMonadReader r (IxRegionT typ s m) where
  ask = lift ask

instance IxPState m => IxPState (IxRegionT typ r m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

