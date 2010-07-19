{-# LANGUAGE TemplateHaskell #-}
module Potential.Array (array, reifyArray) where

import qualified Language.Haskell.TH as TH

import Potential.Array.ArrayQQ
import Potential.Array.CodeGenerator

