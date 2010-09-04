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
	TemplateHaskell,
	TypeFamilies,
	TypeOperators,
	GADTs,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances #-}
module Language.Potential.DataStructure.MetaData
	( NumConstructors(..), AllConstructorsField(..)
	, IsFieldOf(..), (:->)
	, Constructed
	, Access1(..), AccessN(..)
	) where

import Prelude (Integer, undefined, (++), foldl, ($), Maybe(..), fromIntegral)
import Data.Word (Word64(..))

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

import Language.Potential.Size
import Language.Potential.DataStructure.AbstractSyntax (Bit(..))

-- |Type-level tracking of how many constructors the structure 'typ' has.
type family NumConstructors typ

-- |Type-level tracking of the assertion that every constructor of the structure
-- 'typ' has the field 'field_label'.
class AllConstructorsField typ field_label

-- |Given a field label, we need a way to locate the corresponding field.  This
-- structure carries the relevant data in both cases: the case where the
-- structure has only one constructor (so it's unambiguous how to access the
-- field) and where it has many constructors (in which case we'll need to
-- determine which constructor was used in order to figure out how to access
-- the field, if the field is present at all).
data AccessStrategy =
     OneConstr { maskIsolate :: Word64
	       , maskForget  :: Word64
	       , bytesIn     :: Word64
	       , bitsIn      :: Integer }
   | ManyConstr { constrIn :: AccessStrategy
		, strategies :: [ ([Bit], Maybe AccessStrategy) ] }

-- |Used to describe that the label 'field_label' describes a field of type
-- 'FieldType typ field_label' for the structure 'typ'.
class IsFieldOf typ field_label | field_label -> typ
 where
  type FieldType typ field_label
  projField :: typ -> field_label -> FieldType typ field_label
  projField _ _ = undefined
  access :: field_label -> [ AccessStrategy ]

-- |A type for modeling the composition of field labels.
data a :-> b where
 (:->) :: ( IsFieldOf typ1 a , IsFieldOf (FieldType typ1 a) b )
		=> a -> b -> (a :-> b)

instance (IsFieldOf typ1 f1, IsFieldOf (FieldType typ1 f1) f2)
 => IsFieldOf typ1 (f1 :-> f2) where
  type FieldType typ1 (f1 :-> f2) = FieldType (FieldType typ1 f1) f2
  access (f1 :-> f2) = access f1 ++ access f2


-- |Used to access a field in a structure which has precisely one constructor.
class (D1 :== NumConstructors typ) => Access1 typ where
  access1 :: (IsFieldOf typ field_label)
		=> typ -> field_label -> FieldType typ field_label
  access1 _ _ = undefined


-- |Used to access a field in a structure which has more than one constructor.
class (D1 :< NumConstructors typ) => AccessN typ where
  accessN :: (IsFieldOf typ field_label)
		=> Constructed c typ -> field_label -> FieldType typ field_label
  accessN _ _ = undefined


-- |When 'typ' is a child of another structure, 'c' is the name of the field
-- where the constructor of 'typ' is being stored.  Example:
--
--             |---------------|-----------|
--    mod_rep: |    modules    | modules_c |
--             |---------------|-----------|
--
-- We'd expect 'modules' to have type Constructed Modules_c Modules
data Constructed c typ = Constructed c typ


instance THS.Lift AccessStrategy where
  lift a@OneConstr{} = foldl TH.appE [| OneConstr |]
			[ THS.lift (fromIntegral $ maskIsolate a :: Integer)
			, THS.lift (fromIntegral $ maskForget a :: Integer)
			, THS.lift (fromIntegral $ bytesIn a :: Integer)
			, THS.lift $ bitsIn a ]
  lift a@ManyConstr{} = foldl TH.appE [| ManyConstr |]
			[ THS.lift $ constrIn a
			, THS.lift $ strategies a ]


