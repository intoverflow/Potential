{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	NoMonomorphismRestriction,
	UndecidableInstances,
	NoImplicitPrelude,
	FlexibleInstances,
	FlexibleContexts,
	MultiParamTypeClasses,
	TypeFamilies,
	TemplateHaskell,
	QuasiQuotes #-}
module Language.Potential.Arch.Amd64.Machine.IntGate where

import Language.Potential.Size
import Language.Potential.Core
import Language.Potential.Stack
import Language.Potential.Flow
import Language.Potential.Mov

import Language.Potential.Functions (defun)
import Language.Potential.DataStructure

[$struct_diagram|

                        InterruptGate

    |31----------------------------------------------------0|
    |                      reserved                         | 12
    |-------------------------------------------------------|

    |31----------------------------------------------------0|
    |                      offset_hi                        | 8
    |-------------------------------------------------------|

    |-31------16-|-15-|14-13|-12-|11---8|7---5|-4-|-3-|2---0|
    | offset_mid |  p | dpl | 0  | 1110 | 000 | 0 | 0 | ist | 4
    |------------|----|-----|----|------|-----|---|---|-----|
                                    (
                                    (
                                    ( this is the Type field,
                                    ( set up for interrupt gate

    |31------------------------16|15-----------------------0|
    |           segsel           |          offset_lo       | 0
    |----------------------------|--------------------------|

|]


data Present    = Present
data NotPresent = NotPresent
defineDataSize ''Present    1
defineDataSize ''NotPresent 1


-- rax contains Ptr to the interrupt gate
-- will return dpl in rbx
getDPL = defun "getDPL" $
     do lift $ isCode
	proj_InterruptGate_0 rax rbx
	push rcx
	proj_InterruptGate_0_dpl rbx rcx
	pop rcx
	ret

