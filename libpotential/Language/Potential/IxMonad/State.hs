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
module Language.Potential.IxMonad.State
	( IxMonadState(..)
	, IxStateT(..)
	) where

import Prelude( ($) )
import Language.Potential.IxMonad.IxMonad

class (IxMonad m) => IxMonadState s m | m -> s where
  get :: m Unmodeled x x s
  put :: s -> m Unmodeled x x ()

newtype IxStateT s m ct x y a =
    IxStateT { runIxStateT :: s -> m ct x y (a, s) }

instance IxMonadTrans (IxStateT s) where
  lift op = IxStateT $ \s -> fmap (\a -> (a, s)) op

instance IxFunctor m => IxFunctor (IxStateT s m) where
  fmap f m = IxStateT $ \s -> fmap (\(a, s') -> (f a, s')) (runIxStateT m s)

instance IxMonad m => IxMonad (IxStateT s m) where
  unsafeReturn a = lift $ unsafeReturn a
  st >>= f = IxStateT $ \s -> runIxStateT st s >>= \(a, s') ->
			      let st' = f a
			      in runIxStateT st' s'

instance (IxMonad m) => IxMonadState s (IxStateT s m) where
  get   = IxStateT $ \s -> return (s, s)
  put s = IxStateT $ \_ -> return ((), s)

