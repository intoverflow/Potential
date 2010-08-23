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
	QuasiQuotes,
	TypeFamilies,
	NoMonomorphismRestriction #-}
module Language.Potential.Arch.Amd64.Machine.IDT where

import Language.Potential.Array
import Language.Potential.Arch.Amd64.Machine.IntGate

reifyArray ast_InterruptGate [$array|

				InterruptDescriptionTable

		     0 divideError
		     1 reserved
		     2 nMI
		     3 breakpoint

		     4 overflow
		     5 boundRangeExceeded
		     6 invalidOpcode
		     7 noMathCoprocessor

		     8 doubleFault
		     9 coprocessorSegmentOverrun
		    10 invalidTSS
		    11 segmentNotPresent

		    12 stackSegmentFault
		    13 generalProtection
		    14 pageFault
		    15 reserved

		    16 floatingPointError
		    17 alignmentCheck
		    18 machineCheck
		    19 sIMDFloatingPoint

		    -- The rest are reserved, until interrupt 32, which is ours

		    32 myInterrupt
|]

