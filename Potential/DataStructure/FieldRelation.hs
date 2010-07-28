{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Potential.DataStructure.FieldRelation (IsFieldOf(..)) where

import Prelude (Integer)
import Data.Word (Word64(..))

class IsFieldOf partial field_label field_type
  | field_label -> partial
  , partial field_label -> field_type
 where
  forgetMask  :: partial -> field_label -> Word64
  isolateMask :: partial -> field_label -> Word64
  bitOffset   :: partial -> field_label -> Integer
  projField   :: partial -> field_label -> field_type

