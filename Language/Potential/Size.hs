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
	MultiParamTypeClasses,
	FlexibleInstances,
	TypeOperators,
	GADTs,
	ScopedTypeVariables,
	FlexibleContexts,
	TypeFamilies #-}
module Language.Potential.Size where

import Language.Haskell.TH

import Prelude
import Language.Potential.Constraints

class (ToInt (SZ a)) => HasSZ a where
  type SZ a
  sz :: a -> SZ a
  sz _ = undefined

dataSize  f = toInt $ sz f
dataSizeT t = return $ AppE (VarE 'dataSize)
                            (SigE (VarE 'undefined)
                                  (ConT t))

mkT 0 = ConT ''T0
mkT 1 = ConT ''T1
mkT 2 = ConT ''T2
mkT 16 = ConT ''T16
mkT 32 = ConT ''T32
mkT 64 = ConT ''T64
mkT 128 = ConT ''T128
mkT n = AppT (ConT ''S)
             (mkT $ n - 1)

class MaybeHasSZ a c
instance (HasSZ a) => MaybeHasSZ a ConstraintsOn
instance MaybeHasSZ a ConstraintsOff

class a :<= b
instance (:<=) Z b
instance (a :<= b) => (:<=) (S a) (S b)

class (:<=?) a b c
instance (a :<= b) => (:<=?) a b ConstraintsOn
instance (:<=?) a b ConstraintsOff

class a :< b
instance (:<) Z (S Z)
instance (a :< b) => (:<) (S a) (S b)

class (:<?) a b c
instance (a :< b) => (:<?) a b ConstraintsOn
instance (:<?) a b ConstraintsOff

class a :== b
instance (:==) Z Z
instance (a :== b) => (:==) (S a) (S b)

class (:==?) a b c
instance (a :== b) => (:==?) a b ConstraintsOn
instance (:==?) a b ConstraintsOff

class HasIntSZ c where
  intSZ :: c -> Int

data SZAsserted a s where
  SZA :: (SZ a ~ s) => a -> SZAsserted a (SZ a)

instance (ToInt s, HasSZ a) => HasSZ (SZAsserted a s) where
  type SZ (SZAsserted a s) = s


-- Stuff for describing representation size
data Z = Z
data S a = S a

class ToInt a where
  toInt :: a -> Integer
instance ToInt Z where
  toInt _ = 0
instance (ToInt a) => ToInt (S a) where
  toInt (w::(S a)) = 1 + (toInt (undefined :: a))

{- requires undecidable instances
class Add a b where
  type a :+: b

instance Add Z b where
  type Z :+: b = b
instance Add a b => Add (S a) b where
  type (S a) :+: b = a :+: (S b)

class Mul a b where
  type a :*: b

instance Mul Z b where
  type Z :*: b = Z
instance Mul a b => Mul (S a) b where
  type (S a) :*: b = (a :*: b) :+: b
-}

type T0 = Z
type T1 = S Z
type T2 = S (S Z)
type T16 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z))))))))))))))))
type T32 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z))))))))))))))))))))))))))))))))
type T64 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	   S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

type T128 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (
	    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
