{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	NoImplicitPrelude,
	UndecidableInstances,
	MultiParamTypeClasses,
	FlexibleInstances
	#-}
module Language.Potential.IxMonad
	( Composition(..)
	, IxFunctor(..), IxMonad(..), IxMonadTrans(..)
	, Unmodeled, Composable, Terminal
	, unmodeled, composable, terminal
	) where

import Prelude( ($) )
import Data.Monoid

import Language.Potential.IxMonad.IxMonad
import Language.Potential.IxMonad.Constrained
import Language.Potential.IxMonad.Reader
import Language.Potential.IxMonad.Region
import Language.Potential.IxMonad.State
import Language.Potential.IxMonad.Writer
import Language.Potential.IxMonad.PState

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

