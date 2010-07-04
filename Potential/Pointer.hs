{-# LANGUAGE
	NoImplicitPrelude,
	TypeFamilies,
	EmptyDataDecls,
	Rank2Types,
	FlexibleContexts
	#-}
module Potential.Pointer
	( Ptr64, newPtr64, fromPtr64
	, primPtrProj, primPtrInj
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
newPtr64 :: IxMonadWriter [Instr] m
	 => t -> MemRegion r m x x z Composable (Ptr64 r t)
newPtr64 t =
     do lift $ instr Alloc
	return $ Ptr64 t

-- For projecting from Ptr64 r Type to Type_Offset
-- Types are encoded by the proj function
primPtrProj proj offset src dst =
     do instr $ Ld (Deref2 offset (arg src)) (arg dst)
	ptr <- get src
	dat <- fromPtr64 ptr
	set dst (proj dat)

-- For injecting from Type_Offset into Ptr64 s Type, given a Ptr64 r Type
-- to start (this burns up a subregion because it uses smuggleFrom to get
-- the updated result)
primPtrInj inj offset partialSrc structSrc =
     do instr $ Sto (arg partialSrc) (Deref2 offset (arg structSrc))
	ptr <- lift $ get structSrc
	_ <- fromPtr64 ptr
	partial <- lift $ get partialSrc
	ptr' <- nestMemoryRegion $ smuggleFrom (inj partial) (return ptr)
	lift $ set structSrc ptr'

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

