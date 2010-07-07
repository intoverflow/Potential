{-# LANGUAGE
	ScopedTypeVariables,
	NoImplicitPrelude,
	NoMonomorphismRestriction,
	TypeFamilies,
	EmptyDataDecls,
	Rank2Types,
	MultiParamTypeClasses,
	FlexibleContexts,
	FlexibleInstances,
	UndecidableInstances
	#-}
module Potential.Pointer
	( Ptr64, newPtr64, fromPtr64
	, primPtrProj, primPtrInj
	, MemRegion, MemSubRegion
	, withMemoryRegion, nestMemoryRegion
	) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

import Potential.IxMonad.Region
import Potential.IxMonad.Writer

data Memory -- used to tag a region as a Memory region
type MemRegion r m = Region Memory r m
type MemSubRegion r s m ct = SubRegion Memory r s m ct


-- A pointer, bound to region r
data Ptr64 r t = Ptr64 t
instance HasSZ (Ptr64 r t) where type SZ (Ptr64 r t) = T64

getPtrData :: Ptr64 r t -> t
getPtrData _ = undefined

fromPtr64 ptr = return $ getPtrData ptr


-- A type for locking up special instructions to prevent the unwashed
-- masses from using them
type MemSensitive r s m ct x y a =
    MemSubRegion r s m ct x y -> MemRegion s m ct x y a

memSensitive :: MemSensitive r s m ct x y a -> MemSensitive r s m ct x y a
memSensitive a = a


-- Allocate a pointer in the current region
newPtr64' :: IxMonad m => t -> MemRegion r m Composable x x (Ptr64 r t)
newPtr64' t = unsafeReturn $ Ptr64 t

newPtr64InSupRegion :: IxMonad m =>
			MemSubRegion r s m ct x' y' ->
			t -> MemRegion s m Composable x x (Ptr64 r t)
newPtr64InSupRegion _ t = unsafeReturn $ Ptr64 t

newPtr64 t dst initializers =
     do lift $ instr Alloc
	ptr <- newPtr64' t
	lift $ set dst ptr

belongsInSubRegion :: IxMonad m =>
			MemSubRegion r s m ct x' y' ->
			Ptr64 s t ->
			MemRegion s m Unmodeled x x ()
belongsInSubRegion _ _ = return ()

belongsInSupRegion :: IxMonad m =>
			MemSubRegion r s m ct x' y' ->
			Ptr64 r t ->
			MemRegion s m Unmodeled x x ()
belongsInSupRegion _ _ = return ()

-- For projecting from Ptr64 r Type to Type_Offset
-- Types are encoded by the proj function
primPtrProj proj offset src dst =
     do instr $ Ld (Deref2 offset (arg src)) (arg dst)
	ptr <- get src
	dat <- fromPtr64 ptr
	set dst (proj dat)

transform t' (SubRegion sr) =
     do instr TxOwnership
	sr $ newPtr64' t'

-- For injecting from Type_Offset into Ptr64 s Type, given a Ptr64 r Type
primPtrInj inj offset partialSrc structSrc sr =
     do instr TxOwnership
	instr $ Sto (arg partialSrc) (Deref2 offset (arg structSrc))
	partial    <- lift $ get partialSrc
	structPtr  <- lift $ get structSrc
	lift $ forget structSrc
	belongsInSubRegion sr structPtr
	struct     <- fromPtr64 structPtr
	structPtr' <- newPtr64InSupRegion sr (inj partial struct)
	belongsInSupRegion sr structPtr'
	lift $ set structSrc structPtr'


-- the Memory region manager
memRegionMgr :: (IxMonadWriter [Instr] m) => RegionMgr m
memRegionMgr =
      RegionMgr { enter    = instr EnterRegion
		, close    = instr CloseRegion
		, goUp     = instr GoUpRegion
		, comeDown = instr ComeDownRegion
		}

-- Execute code within a memory region
withMemoryRegion :: ( IxMonadWriter [Instr] m
		    , Composition ct Unmodeled, Compose ct Unmodeled ~ ct
		    , Composition Unmodeled ct, Compose Unmodeled ct ~ ct )
		 => (forall r . MemRegion r m ct x y a) -> m ct x y a
withMemoryRegion r = withRegion memRegionMgr r


-- Nest a memory region
nestMemoryRegion :: ( IxMonad m
		    , Composition Unmodeled ct, Compose Unmodeled ct ~ ct
		    , Composition ct Unmodeled, Compose ct Unmodeled ~ ct)
		 => (forall s . MemSubRegion r s m ct x y ->
				     MemRegion s m ct x y a)
		 -> MemRegion r m ct x y a
nestMemoryRegion r = nestRegion r

