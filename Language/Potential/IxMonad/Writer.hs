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
module Language.Potential.IxMonad.Writer
	( IxMonadWriter(..)
	, IxWriterT(..)
	) where

import Prelude( ($) )
import Data.Monoid

import Language.Potential.IxMonad.IxMonad


class (Monoid w, IxMonad m) => IxMonadWriter w m | m -> w where
  tell   :: w -> m Unmodeled x x ()
  -- listen :: m ct a -> m ct (a, w)
  -- pass   :: m ct (a, w -> w) -> m ct a


newtype IxWriterT w m ct x y a =
    IxWriterT { runIxWriterT :: m ct x y (a, w) }

instance (Monoid w) => IxMonadTrans (IxWriterT w) where
  lift op = IxWriterT $ fmap (\a -> (a, mempty)) op

instance (Monoid w, IxFunctor m) => IxFunctor (IxWriterT w m) where
  fmap f m = IxWriterT $ fmap (\(a, w) -> (f a, w)) (runIxWriterT m)

instance (Monoid w, IxMonad m) => IxMonad (IxWriterT w m) where
  unsafeReturn a = lift $ unsafeReturn a
  wr >>= f = IxWriterT $ runIxWriterT wr >>= \(a, w) ->
			  let wr' = f a
			  in fmap (\(b, w') -> (b, w `mappend` w'))
				  (runIxWriterT wr')

instance (Monoid w, IxMonad m) => IxMonadWriter w (IxWriterT w m) where
  tell w = IxWriterT $ return ( (), w )


