{-# LANGUAGE
	ScopedTypeVariables,
	NoImplicitPrelude,
	NoMonomorphismRestriction,
	TypeFamilies,
	EmptyDataDecls,
	Rank2Types,
	MultiParamTypeClasses,
	TypeSynonymInstances,
	FlexibleContexts,
	FlexibleInstances,
	UndecidableInstances,
	ExistentialQuantification
	#-}
module Language.Potential.Pointer
	( Ptr64, newPtr64, fromPtr64
	, FieldUpdate(..)
	, primPtrProj, primPtrInj
	, primFieldProj, primFieldInj
	, primArrayProj, primArrayInj
	, MemRegion, isMemRegion, isMemSubRegion
	, withMemoryRegion, nestMemoryRegion
	) where

import Prelude( ($) )

import Language.Potential.Size
import Language.Potential.Core
import Language.Potential.Assembly
import Language.Potential.DataStructure.FieldRelation

import Language.Potential.IxMonad.Region
import Language.Potential.IxMonad.Reader
import Language.Potential.IxMonad.Writer

data FieldUpdate partial =
  forall field_label x reg . IsFieldOf partial field_label x
  => field_label :<- reg

data Memory -- used to tag a region as a Memory region
type MemRegion r m = Region Memory r m
type MemSubRegionWitness r s = SubRegionWitness Memory r s

isMemRegion :: IxMonad m => m Unmodeled x x () -> MemRegion r m Unmodeled x x ()
isMemRegion _ = return ()

isMemSubRegion :: (IxMonadRegion m, RegionType m ~ Memory)
			=> m Unmodeled x x ()
			-> IxReaderT (MemSubRegionWitness r (RegionLabel m))
					m Unmodeled x x ()
isMemSubRegion _ = return ()

instance IxCode m => IxCode (MemRegion r m) where
  type Constraints (MemRegion r m) = Constraints m

instance (IxCode m, IxMonadRegion m, RegionType m ~ Memory, RegionLabel m ~ s)
  => IxCode (IxReaderT (MemSubRegionWitness r s) m) where
    type Constraints (IxReaderT (MemSubRegionWitness r s) m) = Constraints m

instance ASMable m => ASMable (MemRegion r m) where
  asm constraints mr = asm constraints (withRegion' memRegionMgr mr)

instance (ASMable m, IxMonadRegion m, RegionType m ~ Memory, RegionLabel m ~ s)
  => ASMable (IxReaderT (MemSubRegionWitness r s) m) where
    asm constraints mr = asm constraints (runIxReaderT mr undefined)

-- A pointer, bound to region r
data Ptr64 r t = Ptr64 t
instance HasSZ (Ptr64 r t) where type SZ (Ptr64 r t) = T64

getPtrData :: Ptr64 r t -> t
getPtrData _ = undefined

fromPtr64 ptr = return $ getPtrData ptr


-- Allocate a pointer in the current region
newPtr64' :: IxMonadRegion m => t -> m Composable x x (Ptr64 (RegionLabel m) t)
newPtr64' t = unsafeReturn $ Ptr64 t

newPtr64 t dst =
     do instr Alloc
	ptr <- newPtr64' t
	set dst ptr

-- Pointer operations for dealing with sub and sup regions
newPtr64InSupRegion :: IxMonadReader (MemSubRegionWitness r s) m =>
			t -> m Composable x x (Ptr64 r t)
newPtr64InSupRegion t = unsafeReturn $ Ptr64 t

belongsInSupRegion :: IxMonadReader (MemSubRegionWitness r s) m =>
			Ptr64 r t -> m Unmodeled x x ()
belongsInSupRegion _ = return ()

belongsInSubRegion :: IxMonadReader (MemSubRegionWitness r s) m =>
			Ptr64 s t -> m Unmodeled x x ()
belongsInSubRegion _ = return ()

belongsHere :: (IxMonadRegion m, RegionType m ~ Memory)
		=> Ptr64 (RegionLabel m) t ->  m Unmodeled x x ()
belongsHere _ = return ()

-- For projecting from Ptr64 r Type to Type_Offset
-- Types are encoded by the proj function
primPtrProj proj offset src dst =
     do instr $ Ld (Deref2 offset (arg src)) (arg dst)
	ptr <- get src
	belongsHere ptr
	dat <- fromPtr64 ptr
	set dst (proj dat)

primPtrInj inj offset partialSrc structSrc =
     do instr TxOwnership
	instr $ Sto (arg partialSrc) (Deref2 offset (arg structSrc))
	partial    <- get partialSrc
	structPtr  <- get structSrc
	forget structSrc
	belongsInSubRegion structPtr
	struct     <- fromPtr64 structPtr
	structPtr' <- newPtr64InSupRegion (inj partial struct)
	belongsInSupRegion structPtr'
	set structSrc structPtr'

-- For projecting from a partial to a field
primFieldProj field_label src tmp =
     do forget tmp
	partial <- get src
	forget src
	instr $ MovC (isolateMask partial field_label) (arg tmp)
	instr $ And (arg tmp) (arg src)
	instr $ ShR (bitOffset partial field_label) (arg src)
	let field = projField partial field_label
	set src field

-- For injecting from a field into a partial
primFieldInj inj field_label src dst tmp =
     do constraints <- getConstraints
	forget tmp
	partial <- get dst
	instr $ ShL (bitOffset partial field_label) (arg src)
	instr $ MovC (forgetMask partial field_label) (arg tmp)
	instr $ And (arg tmp) (arg dst)
	instr $ Or (arg src) (arg dst)
	-- this last right shift restores src to its original contents
	instr $ ShR (bitOffset partial field_label) (arg src)
	field <- get src
	set dst $ inj constraints field partial

-- Projects from an array pointer to a cell pointer
primArrayProj proj offset src dst =
     do instr $ Lea (Deref2 offset (arg src)) (arg dst)
	arrayPtr <- get src
	belongsHere arrayPtr
	array <- fromPtr64 arrayPtr
	let cell = proj array
	cellPtr <- newPtr64' cell
	belongsHere cellPtr
	set dst cellPtr
	return ()

primArrayInj inj offset src dst =
     do instr TxOwnership
	instr $ Sto (arg src) (Deref2 offset (arg dst))
	partial  <- get src
	arrayPtr <- get dst
	forget dst
	belongsInSubRegion arrayPtr
	array <- fromPtr64 arrayPtr
	arrayPtr' <- newPtr64InSupRegion (inj partial array)
	belongsInSupRegion arrayPtr'
	set dst arrayPtr'

-- the Memory region manager
memRegionMgr :: (IxMonadWriter [Instr] m) => RegionMgr m
memRegionMgr =
      RegionMgr { enter    = instr NewRegion
		, close    = instr KillRegion
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
		 => (forall s . IxReaderT (MemSubRegionWitness r s)
					  (MemRegion s m) ct x y a)
		 -> MemRegion r m ct x y a
nestMemoryRegion r = nestRegion r

