{-# LANGUAGE
	FlexibleInstances,
	NoImplicitPrelude,
	TypeFamilies #-}
module Potential.PrimTypes
	( RB(..), CB0(..), CB1(..), Int64
	, Stack, splitStack, asStack
	, FrameBasePtr64, makeFramePtr64, getStackFromFramePtr64
	, Ptr64, fromPtr64, updatePtr64
	) where

import Prelude ( Show(..), Int, undefined, ($), id )

import Potential.Size
import Potential.Handles
import Potential.PMonad

-- some basic data types
data RB = RB   deriving Show
data CB0 = CB0 deriving Show
data CB1 = CB1 deriving Show
data Int64 = Int64 Int

data FrameBasePtr64 h s t = FrameBasePtr64 h s t
getBPHandle :: FrameBasePtr64 h s t -> h
getBPHandle _ = undefined
getBPStack :: FrameBasePtr64 h s t -> s
getBPStack _ = undefined

data Stack a b = Stack a b
assertStack :: Stack a b -> Stack a b
assertStack = id
splitStack :: Stack a (Stack a' b') -> (a, Stack a' b')
splitStack _ = (undefined, undefined)
asStack :: a -> b -> Stack a b
asStack _ _ = undefined

data Ptr64 h t = Ptr64 h t
getPtrHandle :: Ptr64 h t -> h
getPtrHandle _ = undefined
getPtrData :: Ptr64 h t -> t
getPtrData _ = undefined

fromPtr64 ptr =
     do _ <- handleIsOpen (getPtrHandle ptr)
	return $ getPtrData ptr

updatePtr64 ptr t' =
     do _ <- handleIsOpen (getPtrHandle ptr)
	free (getPtrHandle ptr)
	h <- alloc
	return $ Ptr64 h t'

makeFramePtr64 sp frame =
     do free (getPtrHandle sp)
	h' <- alloc
	return ( FrameBasePtr64 (getPtrHandle sp)
				(assertStack $ getPtrData sp)
				frame
	       , Ptr64 h' (undefined :: Stack a' b')
	       )

getStackFromFramePtr64 bp =
     do let h = getBPHandle bp
	realloc h
	return $ Ptr64 h (getBPStack bp)

instance HasSZ RB where type SZ RB = T1
instance HasSZ CB0 where type SZ CB0 = T1
instance HasSZ CB1 where type SZ CB1 = T1
instance HasSZ Int64 where type SZ Int64 = T64
instance HasSZ (FrameBasePtr64 h s t) where type SZ (FrameBasePtr64 h s t) = T64
instance HasSZ (Ptr64 h t) where type SZ (Ptr64 h t) = T64

