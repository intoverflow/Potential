{-# LANGUAGE
	EmptyDataDecls,
	NoImplicitPrelude,
	NoMonomorphismRestriction,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	ScopedTypeVariables,
	TypeFamilies,
	TemplateHaskell,
	QuasiQuotes #-}
module Potential.Arch.Amd64.Machine.Flags where

import Potential.Size
import Potential.Assembly
import Potential.Core
import Potential.Integer
import Potential.Flow

import Potential.DataStructure

[$struct_diagram|

		    EFlagsRegister

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

data CF a
data PF a
data AF a
data ZF a
data SF a
data OF a

applyCmp ::
     c -> EFlagsRegister cf pf af zf sf tf if' df of' iopl rf ac vif vip id
       -> EFlagsRegister (CF c) (PF c) (AF c) (ZF c) (SF c) tf if' df (OF c)
			 iopl rf ac vif vip id
applyCmp _ _ = undefined

assertZF :: a -> ZF a -> ()
assertZF _ _ = ()


data PrivLevelUser   = PrivLevelUser
data PrivLevelKernel = PrivLevelKernel
defineDataSize ''PrivLevelUser   2
defineDataSize ''PrivLevelKernel 2

assertPrivLevelKernel =
     do fl <- get rflags
	assertType (proj_EFlagsRegister_iopl fl) PrivLevelKernel
	return ()

cmp r1 r2 =
     do instr $ Cmp (arg r1) (arg r2)
	-- verify we've got integers in these registers
	dr1 <- get r1
	dr2 <- get r2
	assertInt64 dr1
	assertInt64 dr2
	-- increment the machine state's cmp
	c <- get rcmp
	let c' = incCmp c
	set rcmp c'
	-- update the flags register to reflect this
	f <- get rflags
	let f' = applyCmp c' f
	set rflags f'
	get rcmp
	-- return c' -- does not work for some reason

sje c ifZ ifNZ =
     do instr $ SJe ifZ
	fl <- get rflags
	let zf = proj_EFlagsRegister_zf fl
	    _  = assertZF c zf
	primCondJmp (body ifZ) ifNZ

