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
	TypeFamilies,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances #-}
module Language.Potential.DataStructure.MetaData
	( NumConstructors(..), AllConstructorsField(..)
	, IsFieldOf(..), (-->)
	, Access1(..), AccessN(..)
	) where

import Prelude (Integer, undefined, (++))
import Data.Word (Word64(..))

import Language.Potential.Size

-- |Type-level tracking of how many constructors the structure 'typ' has.
type family NumConstructors typ

-- |Type-level tracking of the assertion that every constructor of the structure
-- 'typ' has the field 'field_label'.
class AllConstructorsField typ field_label

-- |Used to describe that the label 'field_label' describes a field of type
-- 'field_typ' for the structure 'typ'.
class IsFieldOf typ field_label field_type
  | field_label -> typ
  , typ field_label -> field_type
 where
  projField :: typ -> field_label -> field_type
  projField _ _ = undefined
  injField :: (IsFieldOf typ' field_label field_type')
		=> typ' -> field_label -> field_typ -> typ
  injField _ _ _ = undefined

-- |A type for modeling the composition of field labels.  Part of the security
-- kernel.
data SubField a b = SubField a b

instance (IsFieldOf typ1 f1 typ2, IsFieldOf typ2 f2 typ3) =>
 IsFieldOf typ1 (SubField f1 f2) typ3 where

-- |Used to describe a sub-field.  Only valid when the parent has exactly one
-- constructor, since this provides no way to determine which constructor has
-- been used.
(-->) :: ( IsFieldOf typ1 f1 typ2
	 , IsFieldOf typ2 f2 typ3
	 , NumConstructors typ1 ~ D1
	 ) => f1 -> f2 -> SubField f1 f2
a --> b = SubField a b


-- |Used to access a field in a structure which has precisely one constructor.
class (D1 :== NumConstructors typ) => Access1 typ where
  access1 :: (IsFieldOf typ field_label field_type)
		=> typ -> field_label -> field_type
  access1 _ _ = undefined


-- |Used to access a field in a structure which has more than one constructor.
class (D1 :< NumConstructors typ) => AccessN typ where
  accessN :: (IsFieldOf typ field_label field_type)
		=> Constructed c typ -> field_label -> field_type
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

