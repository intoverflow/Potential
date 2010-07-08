{-# LANGUAGE TemplateHaskell #-}
module Potential.DataStructure (struct, struct_diagram, defineDataSize) where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.StructQQ
import Potential.DataStructure.StructDiagramQQ
import Potential.DataStructure.CodeGenerator

import Potential.Size (HasSZ, SZ, mkT)

defineDataSize t n =
    return [ TH.InstanceD
                []
                (TH.AppT (TH.ConT ''HasSZ) (TH.ConT t))
                [TH.TySynInstD ''SZ [TH.ConT t] (mkT n)]
           ]



