{-# LANGUAGE TemplateHaskell #-}
module Potential.Array (array, reifyArray, fromInteger) where

import qualified Language.Haskell.TH as TH

import Prelude
import Potential.Array.ArrayQQ
import Potential.Array.CodeGenerator

