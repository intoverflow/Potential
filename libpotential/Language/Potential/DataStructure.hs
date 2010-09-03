{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Standard Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the Potential Standard Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.DataStructure
	( struct_diagram, defineDataSize
	, projField, injField
	, (-->)
	, access1, accessN
	) where

import qualified Language.Haskell.TH as TH

-- import Language.Potential.DataStructure.StructQQ
import Language.Potential.DataStructure.StructDiagramQQ
import Language.Potential.DataStructure.CodeGenerator
import Language.Potential.DataStructure.MetaData

