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
{-# LANGUAGE
	EmptyDataDecls,
	NoImplicitPrelude,
	NoMonomorphismRestriction,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	ScopedTypeVariables,
	TypeFamilies,
	UndecidableInstances,
	TemplateHaskell,
	QuasiQuotes #-}
module Language.Potential.Arch.Amd64.Machine.Flags where

import Language.Potential.Size
import Language.Potential.Assembly
import Language.Potential.Core
import Language.Potential.Integer
import Language.Potential.Flow

import Language.Potential.DataStructure

[$struct_diagram| EFlagsRegister

		    EFlagsRegister (0)

    |63---------------------------------------32|
    |                  reserved                 | 4
    |-------------------------------------------|

    |31------22|-21--|-20--|-19--|-18-|-17-|-16-|
    | reserved | idf | vip | vif | ac | 0  | rf | 2
    |----------|-----|-----|-----|----|----|----|
                  (     (     (    (    (    ( resume
                  (     (     (    (    ( v8086
                  (     (     (    ( alignment check
                  (     (     ( virtual interrupt
                  (     ( virtual interrupt pending
                  ( identification

    |-15-|-14-|13--12|--11--|--10--|--9---|--8--|
    | 0  | 0  | iopl |  ovf |  df  | ief  | tf  | 1
    |----|----|------|------|------|------|-----|
           (     (       (     (      (     ( trap
           (     (       (     (      ( interrupts enabled
           (     (       (     ( direction
           (     (       ( overflow
           (     ( current priv level
           ( nested task

    |---7--|--6--|-5-|---4--|-3-|--2--|-1-|--0--|
    |  sgf |  zf | 0 |  ajf | 0 |  pf | 1 |  cf | 0
    |------|-----|---|------|---|-----|---|-----|
       ( sign  ( zero    ( adjust  ( parity  ( carry

|]

data CS a

incCmp :: a -> (CS a)
incCmp _ = undefined

data CF
defineDataSize ''CF 1
data PF
defineDataSize ''PF 1
data AF
defineDataSize ''AF 1
data ZF
defineDataSize ''ZF 1
data SF
defineDataSize ''SF 1
data OVF
defineDataSize ''OVF 1

applyCmp ::
	EFlagsRegister cf pf af zf sf tf ief df ovf iopl rf ac vif vip idf
     -> EFlagsRegister CF PF AF ZF SF tf ief df OVF iopl rf ac vif vip idf
applyCmp _ = undefined

assertZF :: IxMonad m => ZF -> m Unmodeled x x ()
assertZF _ = return ()


data PrivLevelUser   = PrivLevelUser
data PrivLevelKernel = PrivLevelKernel
defineDataSize ''PrivLevelUser   2
defineDataSize ''PrivLevelKernel 2

assertPrivLevelKernel =
     do flags <- get rflags
	let cpl = projField flags Iopl
	assertType cpl PrivLevelKernel
	return ()

data JmpStyle a = JZ a | JNZ a

compare r1 r2 jmper rest =
     do -- Verify these registers contain integers
	a <- get r1
	b <- get r2
	assertInt64 a
	assertInt64 b
	-- Perform the comparison
	instr $ Cmp (arg r1) (arg r2)
	-- Now run down the cases
	case jmper of
	  JZ a  -> sje a rest
	  JNZ a -> sjne a rest

sje ifZ ifNZ =
     do instr $ SJe ifZ
	primCondJmp (body ifZ) ifNZ

sjne ifNZ ifZ =
     do instr $ SJne ifNZ
	primCondJmp (body ifNZ) ifZ

