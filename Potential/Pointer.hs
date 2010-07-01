{-# LANGUAGE
	NoImplicitPrelude,
	TypeFamilies,
	EmptyDataDecls,
	Rank2Types,
	FlexibleContexts
	#-}
module Potential.Pointer
	( Ptr64, newPtr64, fromPtr64
	, MemRegion, MemSubRegion
	, withMemoryRegion, nestMemoryRegion, smuggleFrom
	) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

import Potential.IxMonad.Region
import Potential.IxMonad.Writer

data Memory -- used to tag a region as a Memory region
type MemRegion r m = Region Memory r m
type MemSubRegion r s m x y = SubRegion Memory r s m x y


-- A pointer, bound to region r
data Ptr64 r t = Ptr64 t
instance HasSZ (Ptr64 r t) where type SZ (Ptr64 r t) = T64

getPtrData :: Ptr64 r t -> t
getPtrData _ = undefined

fromPtr64 ptr = return $ getPtrData ptr


-- Allocate a pointer in the current region
newPtr64 :: t -> MemRegion r (Code c) x x z Composable (Ptr64 r t)
newPtr64 t =
     do lift $ instr Alloc
	return $ Ptr64 t


-- the Memory region manager
memRegionMgr :: (IxMonadWriter [Instr] m) => RegionMgr m
memRegionMgr =
      RegionMgr { enter    = instr EnterRegion
		, close    = instr CloseRegion
		, goUp     = instr GoUpRegion
		, comeDown = instr ComeDownRegion
		}


-- Execute code within a memory region
withMemoryRegion :: IxMonadWriter [Instr] m
		 => (forall r . MemRegion r m x y y Composable a)
		 -> m x y z Composable a
withMemoryRegion r = withRegion memRegionMgr r


-- Nest a memory region
nestMemoryRegion :: IxMonad m
		 => (forall s . MemSubRegion r s m x y y
				-> MemRegion s m x y y Composable a)
		 -> MemRegion r m x y z Composable a
nestMemoryRegion r = nestRegion r


-- Modify types while pulling a pointer from a child to a parent memory region
smuggleFrom :: IxMonadWriter [Instr] m
	    => (t -> t')
	    -> MemRegion s m x y y Composable (Ptr64 s t)
	    -> MemSubRegion r s m y y y
	    -> MemRegion s m x y y Composable (Ptr64 r t')
smuggleFrom f r sr =
     do (Ptr64 t) <- r
	inSupRegion sr $ do instr TxOwnership
			    return $ Ptr64 $ f t

