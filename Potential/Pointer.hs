{-# LANGUAGE
	NoImplicitPrelude,
	TypeFamilies,
	EmptyDataDecls,
	Rank2Types,
	FlexibleContexts
	#-}
module Potential.Pointer
	( Ptr64, newPtr64, fromPtr64, updatePtr64, getPtrData
	, MemRegion, MemSubRegion, withMemoryRegion, nestMemoryRegion
	) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

import Potential.IxMonad.Region
import Potential.IxMonad.Writer

data Memory -- used to tag a region as a Memory region
type MemRegion r m = Region Memory r m
type MemSubRegion r s m x y = SubRegion Memory r s m x y

-- A pointer which is bound to region r
data Ptr64 r t = Ptr64 t
instance HasSZ (Ptr64 r t) where type SZ (Ptr64 r t) = T64

-- Allocate a pointer in the current region
newPtr64 :: t -> MemRegion r (Code c) x x Composable (Ptr64 r t)
newPtr64 t =
     do lift $ instr Alloc
	return $ Ptr64 t

memRegionMgr :: (IxMonadWriter [Instr] m) => RegionMgr m
memRegionMgr =
      RegionMgr { enter    = instr EnterRegion
		, close    = instr CloseRegion
		, goUp     = instr GoUpRegion
		, comeDown = instr ComeDownRegion
		}

withMemoryRegion :: IxMonadWriter [Instr] m
		 => (forall r . MemRegion r m x y Composable a)
		 -> m x y Composable a
withMemoryRegion r = withRegion memRegionMgr r

nestMemoryRegion :: IxMonad m
		 => (forall s . MemSubRegion r s m x y
				-> MemRegion s m x y Composable a)
		 -> MemRegion r m x y Composable a
nestMemoryRegion r = nestRegion r

getPtrData :: Ptr64 r t -> t
getPtrData _ = undefined

fromPtr64 ptr = return $ getPtrData ptr

updatePtr64 :: IxMonad m => Ptr64 r t -> t' -> m x x Composable (Ptr64 r t')
updatePtr64 ptr t' =
     do return $ Ptr64 t'

