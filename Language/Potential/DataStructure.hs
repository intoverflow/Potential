{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.DataStructure
	( struct, struct_diagram, defineDataSize ) where

import qualified Language.Haskell.TH as TH

import Language.Potential.DataStructure.StructQQ
import Language.Potential.DataStructure.StructDiagramQQ
import Language.Potential.DataStructure.CodeGenerator

