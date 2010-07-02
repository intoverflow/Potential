{-# LANGUAGE
	EmptyDataDecls,
	NoImplicitPrelude,
	MultiParamTypeClasses,
	FlexibleInstances,
	TypeFamilies,
	TemplateHaskell,
	QuasiQuotes #-}
module Potential.Machine.Flags where

import Prelude ( fromInteger, undefined, ($) )

import Potential.Size
import Potential.Assembly
import Potential.Core
import Potential.Integer
import Potential.Flow

import Potential.DataStructures

-- example:
-- :info FlagsRegister
[$struct| FlagsRegister where
    cf :: 1	-- carry
    const 1
    pf :: 1	-- parity
    const 0
    af :: 1	-- adjust
    const 0
    zf :: 1	-- zero
    sf :: 1	-- sign
    tf :: 1	-- trap
    ief :: 1	-- interrupt enable
    df :: 1	-- direction
    ovf :: 1	-- overflow
    iopl :: 2	-- current priv level
    const 0	-- nested task, not supported in x86-64 so we make it 0
    const 0
    rf :: 1	-- resume
    const 0	-- virtual 8086, always zero in IA-32e
    ac :: 1	-- alignment check
    vif :: 1	-- virtual interrupt
    vip :: 1	-- virtual interrupt pending
    idf :: 1	-- identification
    reserved 10
    reserved 32
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
     c -> FlagsRegister cf pf af zf sf tf if' df of' iopl rf ac vif vip id
       -> FlagsRegister (CF c) (PF c) (AF c) (ZF c) (SF c) tf if' df (OF c)
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
	assertType (proj_iopl fl) PrivLevelKernel

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
	-- return c' -- for some insane reason, this does not work.

sje fn c =
     do instr $ SJe fn
	fl <- get rflags
	let zf = proj_zf fl
	    _  = assertZF c zf
	primCondJmp (body fn)

