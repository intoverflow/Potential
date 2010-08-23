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
module Language.Potential.IxMonad.Reader
	( IxMonadReader(..)
	, IxReaderT(..)
	) where

import Prelude( ($) )

import Language.Potential.IxMonad.IxMonad


class (IxMonad m) => IxMonadReader r m | m -> r where
  ask :: m Unmodeled x x r

newtype IxReaderT r m ct x y a =
    IxReaderT { runIxReaderT :: r -> m ct x y a }

instance IxMonadTrans (IxReaderT r) where
  lift op = IxReaderT $ \_ -> op

instance IxFunctor m => IxFunctor (IxReaderT r m) where
  fmap f m = IxReaderT $ \r -> fmap f (runIxReaderT m r)

instance IxMonad m => IxMonad (IxReaderT r m) where
  unsafeReturn a = lift $ unsafeReturn a
  rd >>= f = IxReaderT $ \r -> runIxReaderT rd r >>= \a -> runIxReaderT (f a) r

instance IxMonad m => IxMonadReader r (IxReaderT r m) where
  ask = IxReaderT $ \r -> return r


