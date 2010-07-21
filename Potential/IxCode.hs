{-# LANGUAGE TypeFamilies #-}
module Potential.IxCode where

import Potential.IxMonad (IxMonad)

class IxMonad m => IxCode m where
  type Constraints m

