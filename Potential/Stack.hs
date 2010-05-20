{-# LANGUAGE
	NoImplicitPrelude,
	TypeFamilies,
	FlexibleContexts #-}
module Potential.Stack where

import Prelude( ($) )

import Potential.Size
import Potential.PrimTypes

import Potential.PMonad

assertPushableSize :: (SZ a' :<=? T64) c => a' -> PState l c x x ()
assertPushableSize _ = return ()

primPush a' sp =
     do assertPushableSize a'
	stack <- fromPtr64 sp
	updatePtr64 sp $ asStack a' stack

primPop sp =
     do stack <- fromPtr64 sp
	let (a, stack') = splitStack stack
	sp' <- updatePtr64 sp stack'
	return (a, sp')

