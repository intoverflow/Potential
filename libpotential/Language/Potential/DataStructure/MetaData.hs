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
	FunctionalDependencies,
	TemplateHaskell,
	TypeFamilies,
	TypeOperators,
	GADTs,
	Rank2Types,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances #-}
module Language.Potential.DataStructure.MetaData
	( NumConstructors(..), AllConstructorsField(..)
	, IsField(..), (:->), (-->)
	, AccessStrategy(..)
	, Constructed, constructor, Constructor, accessConstructor
	, boundType
	) where

import Prelude
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
	       , bitsIn      :: Integer
	       , accessor_name  :: String }
   | ManyConstr { constrIn :: [ AccessStrategy ]
		, strategies :: [ ([Bit], Maybe AccessStrategy) ]
		, accessor_name :: String }

instance Show AccessStrategy where
  show as@OneConstr{} = accessor_name as
  show as@ManyConstr{} = accessor_name as ++ " via " ++ show (constrIn as)

-- |Used to describe that the label 'field_label' describes a field of type
-- 'FieldType field_label' for the structure 'StructType field_label'.  A
-- minimal definition defines 'access' and 'FieldType'.
class IsField struct_type field_label | field_label -> struct_type
 where
  type FieldType struct_type field_label
  projField :: struct_type -> field_label -> FieldType struct_type field_label
  projField _ _ = undefined
  assertIsFieldOf :: struct_type -> field_label -> ()
  assertIsFieldOf _ _ = ()
  access :: struct_type -> field_label -> [ AccessStrategy ]

-- |A type for modeling the composition of field labels.
data a :-> b where SubField :: a -> b -> a :-> b

-- |A wrapper for the 'SubField' constructor of ':->'
a --> b = SubField a b

instance (IsField sa a, IsField (FieldType sa a) b)
 => IsField sa (a :-> b) where
  type FieldType sa (a :-> b) = FieldType (FieldType sa a) b
  projField sa (SubField a b) = projField (projField sa a) b
  access sa (SubField a b) = access sa a ++ access (projField sa a) b


-- |When 'typ' is a child of another structure, 'c' is the name of the field
-- where the constructor of 'typ' is being stored.  Example:
--
--             |---------------|-----------|
--    Mod_rep: |    modules    | modules_c |
--             |---------------|-----------|
--
-- We'd expect 'modules' to have type
-- 'Constructed (Mod_rep a b) Modules_c ModulesTyp'.
data (IsField cs c) => Constructed cs c typ = Constructed c typ

-- |Used to indicate that the field with this type carries a constructor for
-- field 'l'.  Example:
--
--             |---------------|-----------|
--    Mod_rep: |    modules    | modules_c |
--             |---------------|-----------|
--
-- We'd expect 'modules_c' to have type
-- 'Constructor (Mod_rep a b) Modules'
data (IsField cs l) => Constructor cs l = Constructor l

-- |A type-level function for getting the label for a constructor
constructor :: Constructed cs c typ -> c
constructor _ = undefined

-- |A type-level function for getting the generic type of the struct which holds
-- the container
container :: Constructed cs c typ -> cs
container _ = undefined

-- |A type-level function for validating that a Constructor/Constructed pair
-- matches
boundType :: ( FieldType a l ~ Constructed cs c typ
	     , FieldType a c ~ Constructor cs l
	     ) => l -> typ -> c -> cs -> a -> a
boundType _ _ _ _ a = a

-- |Given a Constructed c typ, get the access strategy for the constructor
accessConstructor :: (IsField cs c) => Constructed cs c typ -> [AccessStrategy]
accessConstructor c = access (container c) (constructor c)

instance THS.Lift AccessStrategy where
  lift a@OneConstr{} = foldl TH.appE [| OneConstr |]
			[ THS.lift (fromIntegral $ maskIsolate a :: Integer)
			, THS.lift (fromIntegral $ maskForget a :: Integer)
			, THS.lift (fromIntegral $ bytesIn a :: Integer)
			, THS.lift $ bitsIn a
			, THS.lift $ accessor_name a ]
  lift a@ManyConstr{} = foldl TH.appE [| ManyConstr |]
			[ THS.lift $ constrIn a
			, THS.lift $ strategies a
			, THS.lift $ accessor_name a ]


