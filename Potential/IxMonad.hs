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

-- Lift IxReaderT
instance (IxMonadState s m) => IxMonadState s (IxReaderT r m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxReaderT r m) where
  tell w = lift $ tell w

instance IxPState m => IxPState (IxReaderT r m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

-- Lift IxWriterT
instance (Monoid w, IxMonadState s m) => IxMonadState s (IxWriterT w m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadReader r m) => IxMonadReader r (IxWriterT w m) where
  ask = lift ask

instance (Monoid w, IxPState m) => IxPState (IxWriterT w m) where
  psGet = lift psGet
  psPut a = lift (psPut a)


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


-- Lift IxRegionT
instance (IxMonadState s m) => IxMonadState s (IxRegionT typ r m) where
  get   = lift get
  put s = lift $ put s
instance (IxMonadState s m) => IxMonadState s (IxSubRegionT typ r s m) where
  get   = lift get
  put s = lift $ put s

instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxRegionT typ r m) where
  tell w = lift $ tell w
instance (Monoid w, IxMonadWriter w m) => IxMonadWriter w (IxSubRegionT typ r s m) where
  tell w = lift $ tell w

instance (IxMonadReader r m) => IxMonadReader r (IxRegionT typ s m) where
  ask = lift ask
instance (IxMonadReader r m) => IxMonadReader r (IxSubRegionT typ s t m) where
  ask = lift ask

instance IxPState m => IxPState (IxRegionT typ r m) where
  psGet = lift psGet
  psPut a = lift (psPut a)
instance IxPState m => IxPState (IxSubRegionT typ r s m) where
  psGet = lift psGet
  psPut a = lift (psPut a)

