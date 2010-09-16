{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Standard Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the Potential Standard Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	NoImplicitPrelude,
	MultiParamTypeClasses,
	FlexibleContexts,
	TypeFamilies
	#-}
module Language.Potential.Label
	( PotentialLabel, LabelGen, runLabel
	, mkLabel, label, ljne, ljmp
	) where

import Prelude( String, Integer, fromInteger, ($), (+), (++), show )

import Language.Potential.Assembly
import Language.Potential.IxMonad.IxMonad
import Language.Potential.IxMonad.State
import Language.Potential.IxMonad.Writer

data PotentialLabel = PotentialLabel String

-- |Used to record a label
label (PotentialLabel s) = tell [Label s]

-- |Used to jump-not-equal to a label
ljne (PotentialLabel s) = tell [LJne s]

-- |Used to jump-not-equal to a label
ljmp (PotentialLabel s) = tell [LJmp s]

data LabelGenState = LabelGenState String Integer
type LabelGen m = IxStateT LabelGenState m

runLabel :: IxMonad m
		=> String -> LabelGen m ct x y a -> m ct x y (a, LabelGenState)
runLabel s m = runIxStateT m (LabelGenState s 0)

incLabel :: IxMonadState LabelGenState m => m Unmodeled x x Integer
incLabel =
     do lgs <- get
	let LabelGenState s n = lgs
	put (LabelGenState s $ n+1)
	return n

nmLabel :: IxMonadState LabelGenState m => m Unmodeled x x String
nmLabel =
     do lgs <- get
	let LabelGenState s _ = lgs
	return s

-- |Used to generate a label
mkLabel :: IxMonadState LabelGenState m => m Unmodeled x x PotentialLabel
mkLabel =
     do n <- incLabel
	s <- nmLabel
	return (PotentialLabel $ s ++ "_" ++ show n)

