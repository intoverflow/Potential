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
	, IsField(..), ProjField(..), GetConstructorData(..)
	, AccessWithConstr(..), DeepAccess(..)
	, AccessStrategy(..)
	, (:->), (-->)
	, Constructed(..), Constructor(..), NotConstructed(..)
	) where

import Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

import Language.Potential.Size
import Language.Potential.DataStructure.AbstractSyntax
	(FieldAccess(..), Bit(..))

-- |When 'typ' is a child of another structure, 'c' is the name of the field
-- where the constructor of 'typ' is being stored.  Example:
--
--             |---------------|-----------|
--    Mod_rep: |    modules    | modules_c |
--             |---------------|-----------|
--
-- We'd expect 'modules' to have type
-- 'Constructed Modules_c ModulesTyp'.
data (D1 :< NumConstructors typ) => Constructed c typ = Constructed c typ

-- |Used to indicate that the field with this type carries a constructor for
-- field 'l'.  Example:
--
--             |---------------|-----------|
--    Mod_rep: |    modules    | modules_c |
--             |---------------|-----------|
--
-- We'd expect 'modules_c' to have type
-- 'Constructor Modules'
data Constructor l = Constructor l

-- |Used to indicate that the given type has no constructors.
data (D1 :== NumConstructors typ) => NotConstructed typ = NotConstructed typ

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
     OneConstr { access_params :: FieldAccess
	       , accessor_name  :: String }
   | ManyConstr { strategies :: [ ([Bit], Maybe FieldAccess) ]
		, accessor_name :: String }
   | WithConstr { constr_access :: AccessStrategy
		, accessor      :: AccessStrategy
		, accessor_name :: String }

instance Show AccessStrategy where
  show as@OneConstr{} = accessor_name as
  show as@ManyConstr{} = accessor_name as ++ " (with strategies)"
  show as@WithConstr{} = accessor_name as ++
			 " via (" ++ show (constr_access as) ++ ")"

-- |The sub-field relation.  Used to access deep within sub-fields of a
-- structure.
data a :-> b = SubField a b

-- |A prettier looking wrapper for the 'SubField' constructor.
(-->) :: a -> b -> a :-> b
a --> b = SubField a b

-- |This class encodes how to locate constructor data for a field.
class GetConstructorData parent_type field_type
 where
  type ConstructorLabel parent_type field_type
  constructorLabel :: parent_type -> field_type
			-> ConstructorLabel parent_type field_type
  constructorLabel _ _ = undefined
  accessConstructor :: parent_type -> field_type -> Maybe AccessStrategy

instance (IsField parent_type constr_label)
 => GetConstructorData parent_type (Constructed constr_label typ)
 where
  type ConstructorLabel parent_type (Constructed constr_label typ) =constr_label
  accessConstructor ps ft = Just $ access ps (constructorLabel ps ft)

-- |A dummy type to be used when there is no constructor for the type we are
-- using.
data NoConstructorLabel = NoConstructorLabel

instance GetConstructorData parent_type (NotConstructed typ)
 where
  type ConstructorLabel parent_type (NotConstructed typ) = NoConstructorLabel
  accessConstructor _ _ = Nothing


-- |Used to describe that the label 'field_label' describes a field of type
-- 'FieldType field_label' for the structure 'StructType field_label'.  A
-- minimal definition defines 'access' and 'FieldType'.
class IsField struct_type field_label
 | field_label -> struct_type
 where
  assertIsFieldOf :: struct_type -> field_label -> ()
  assertIsFieldOf _ _ = ()
  access :: struct_type -> field_label -> AccessStrategy

-- |Type-level functions for accessing sub-field types
class ProjField struct_type field_label
 where
  type FieldType struct_type field_label
  projField :: struct_type -> field_label -> FieldType struct_type field_label
  projField _ _ = undefined

instance ( ProjField struct_type a
	 , ProjField (FieldType struct_type a) b )
 => ProjField struct_type (a :-> b)
 where type FieldType struct_type (a :-> b) =
	FieldType (FieldType struct_type a) b

-- |This class augments the usual 'access' method of 'IsField' with information
-- about constructors.
class ( IsField struct_type field_label
      , ProjField struct_type field_label
      , GetConstructorData struct_type (FieldType struct_type field_label) )
 => AccessWithConstr struct_type field_label
 where
  accessWithConstr :: struct_type -> field_label -> AccessStrategy
  accessWithConstr st fl =
    let a = access st fl
	field_type = projField st fl
    in case (accessConstructor st field_type) of
	  Nothing -> a
	  Just c  -> WithConstr { constr_access = c
				, accessor = a
				, accessor_name = accessor_name a
				}

instance ( IsField struct_type field_label
	 , ProjField struct_type field_label
	 , GetConstructorData struct_type (FieldType struct_type field_label) )
 => AccessWithConstr struct_type field_label

-- |The general purpose, multi-layered field accessor class.
class DeepAccess struct_type field_label
 where
  deepAccess :: struct_type -> field_label -> [ AccessStrategy ]

instance ( ProjField struct_type a
	 , DeepAccess struct_type a
	 , AccessWithConstr (FieldType struct_type a) b )
 => DeepAccess struct_type (a :-> b)
 where
  deepAccess st (SubField a b) = (deepAccess st a) ++
				 [accessWithConstr (projField st a) b]

instance THS.Lift AccessStrategy
 where
  lift a@OneConstr{} = foldl TH.appE [| OneConstr |]
			[ THS.lift $ access_params a
			, THS.lift $ accessor_name a ]
  lift a@ManyConstr{} = foldl TH.appE [| ManyConstr |]
			[ THS.lift $ strategies a
			, THS.lift $ accessor_name a ]
  lift a@WithConstr{} = foldl TH.appE [| WithConstr |]
			[ THS.lift $ constr_access a
			, THS.lift $ accessor a
			, THS.lift $ accessor_name a ]

