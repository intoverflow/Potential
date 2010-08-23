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
	EmptyDataDecls,
	TypeFamilies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FunctionalDependencies
	#-}
module Language.Potential.IxMonad.IxMonad
	( Composition(..)
	, IxFunctor(..), IxMonad(..), IxMonadTrans(..)
	, Unmodeled, Composable, Terminal
	, unmodeled, composable, terminal
	) where

import Prelude( String )

class Composition (fr :: *) (to :: *) where type Compose fr to :: *

data Unmodeled
unmodeled :: IxMonad m => m Unmodeled x x a -> m Unmodeled x x a
unmodeled m = m

data Composable
composable :: IxMonad m => m Composable x y a -> m Composable x y a
composable m = m

data Terminal
terminal :: IxMonad m => m Terminal x y a -> m Terminal x y a
terminal m = m

instance Composition Unmodeled Unmodeled where
  type Compose Unmodeled Unmodeled = Unmodeled
instance Composition Unmodeled Composable where
  type Compose Unmodeled Composable = Composable
instance Composition Unmodeled Terminal where
  type Compose Unmodeled Terminal = Terminal

instance Composition Composable Unmodeled where
  type Compose Composable Unmodeled = Composable
instance Composition Composable Composable where
  type Compose Composable Composable = Composable
instance Composition Composable Terminal where
  type Compose Composable Terminal = Terminal

instance Composition Terminal Unmodeled where
  type Compose Terminal Unmodeled = Terminal


class IxFunctor m where
  fmap :: (a -> b) -> m ct x y a -> m ct x y b

class IxFunctor m => IxMonad m where
  -- minimal interface
  (>>=)  :: Composition ct ct' =>
	    m ct x y a -> (a -> m ct' y z b) ->
	    m (Compose ct ct') x z b
  unsafeReturn :: a -> m ct x y a
  -- stuff for free
  (>>)   :: Composition ct ct' =>
	    m ct x y a -> m ct' y z b ->
	    m (Compose ct ct') x z b
  a >> b = a >>= (\_ -> b)
  return :: a -> m Unmodeled x x a
  return a = unsafeReturn a
  fail :: String -> m ct x x ()
  fail = fail

class IxMonadTrans t where
  lift :: IxMonad m => m ct x y a -> t m ct x y a

