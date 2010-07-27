{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Potential.DataStructure.FieldRelation (IsFieldOf(..)) where

import Data.Word (Word64(..))

class IsFieldOf partial field | field -> partial where
  forgetMask  :: partial -> field -> Word64
  isolateMask :: partial -> field -> Word64

