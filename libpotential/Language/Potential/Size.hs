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
	TemplateHaskell,
	TypeOperators,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	TypeFamilies,
	UndecidableInstances #-}
module Language.Potential.Size
	( D0, D1, D2, D3, D4
	, D5, D6, D7, D8, D9
	, toInt
	, (:*), HasSZ(..), MaybeHasSZ
	, (:<), (:<?), (:==), (:==?)
	, dataSize, dataSizeT, mkSize
	) where

import Language.Haskell.TH
import Data.TypeLevel

import Prelude
import Language.Potential.Constraints

class (Nat (SZ a)) => HasSZ a where
  type SZ a
  sz :: a -> SZ a
  sz _ = undefined

dataSize  f = toInt $ sz f
dataSizeT t = return $ AppE (VarE 'dataSize)
                            (SigE (VarE 'undefined)
                                  (ConT t))

mkSize 0 = ConT ''D0
mkSize 1 = ConT ''D1
mkSize 2 = ConT ''D2
mkSize 3 = ConT ''D3
mkSize 4 = ConT ''D4
mkSize 5 = ConT ''D5
mkSize 6 = ConT ''D6
mkSize 7 = ConT ''D7
mkSize 8 = ConT ''D8
mkSize 9 = ConT ''D9
mkSize n = let ones = n `Prelude.mod` 10
	       rest = n `Prelude.div` 10
	       ones_rep = mkSize ones
	   in if rest (Prelude.==) 0
		then ones_rep
		else AppT (AppT (ConT ''(:*)) (mkSize rest)) ones_rep

class MaybeHasSZ a c
instance (HasSZ a) => MaybeHasSZ a ConstraintsOn
instance MaybeHasSZ a ConstraintsOff

class a :< b
instance (Trich a b LT) => (:<) a b

class (:<?) a b c
instance (a :< b) => (:<?) a b ConstraintsOn
instance (:<?) a b ConstraintsOff

class a :== b
instance (Trich a b EQ) => (:==) a b

class (:==?) a b c
instance (a :== b) => (:==?) a b ConstraintsOn
instance (:==?) a b ConstraintsOff

{- This causes "duplicate instance declarations."
class a :<= b
instance (Trich a b LT) => (:<=) a b
instance (Trich a b EQ) => (:<=) a b

class (:<=?) a b c
instance (a :<= b) => (:<=?) a b ConstraintsOn
instance (:<=?) a b ConstraintsOff
-}

