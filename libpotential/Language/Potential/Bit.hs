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
	FlexibleInstances,
	TypeFamilies #-}
module Language.Potential.Bit
	( RB(..), CB0(..), CB1(..) ) where

import Prelude( Show )
import Language.Potential.Size

-- some basic data types
data RB = RB   deriving Show
data CB0 = CB0 deriving Show
data CB1 = CB1 deriving Show

instance HasSZ RB where type SZ RB = D1
instance HasSZ CB0 where type SZ CB0 = D1
instance HasSZ CB1 where type SZ CB1 = D1

