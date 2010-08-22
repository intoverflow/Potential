{-# LANGUAGE
	FlexibleInstances,
	TypeFamilies #-}
module Language.Potential.Bit
	( RB(..), CB0(..), CB1(..) ) where

import Prelude( Show )
import Language.Potential.Size

-- some basic data types
data RB = RB   deriving Show
data CB0 = CB0 deriving Show
data CB1 = CB1 deriving Show

instance HasSZ RB where type SZ RB = T1
instance HasSZ CB0 where type SZ CB0 = T1
instance HasSZ CB1 where type SZ CB1 = T1

