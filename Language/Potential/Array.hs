{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.Array (array, reifyArray, fromInteger) where

import qualified Language.Haskell.TH as TH

import Prelude
import Language.Potential.Array.ArrayQQ
import Language.Potential.Array.CodeGenerator

