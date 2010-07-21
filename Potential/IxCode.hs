{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Potential.IxCode where

import Potential.IxMonad (IxMonad)

class IxMonad m => IxCode m where
  type Constraints m

class IxCode m => ASMable m i where
  asm :: Constraints m -> m ct x y a -> [i]

