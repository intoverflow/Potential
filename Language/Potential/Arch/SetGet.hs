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
	TypeFamilies,
	MultiParamTypeClasses,
	FunctionalDependencies #-}
module Language.Potential.Arch.SetGet where

class Setter field new ms where
  type Set field new ms
  set' :: field -> new -> ms -> Set field new ms

class Getter field ms where
  type Get field ms
  get' :: field -> ms -> Get field ms

class IsArg field reg | field -> reg where
  arg   :: field -> reg
  isArg :: field -> ()


