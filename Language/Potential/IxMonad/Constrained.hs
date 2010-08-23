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

