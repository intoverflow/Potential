{-# LANGUAGE TemplateHaskell #-}
module Potential.DataStructure (struct, struct_diagram, defineDataSize) where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.StructQQ
import Potential.DataStructure.StructDiagramQQ
import Potential.DataStructure.CodeGenerator

