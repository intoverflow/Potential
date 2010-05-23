{-# LANGUAGE
	TypeFamilies,
	NoImplicitPrelude #-}
module Potential.Pointer
	( assertPtrIsValid, Ptr64(..) , fromPtr64 , updatePtr64
	, getPtrHandle, getPtrData
	) where

import Prelude( ($) )

import Potential.Size
import Potential.Core

data Ptr64 h t = Ptr64 h t
instance HasSZ (Ptr64 h t) where type SZ (Ptr64 h t) = T64

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

assertPtrIsValid src =
     do assertRegister src
	ptr <- get src
	_ <- fromPtr64 ptr
	return ()

