{-# LANGUAGE
	Rank2Types,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	TypeFamilies,
	TemplateHaskell #-}
module Potential.BuildArray
	( Array
	, Cell(..)
	, defineArray
	) where

import Language.Haskell.TH

import Potential.Core hiding (return, (>>), (>>=), fail)
import qualified Potential.Core as Core

import Potential.Size
import Potential.DataStructures
import Potential.Bit
import Potential.Pointer

data Array t s =
     Ar { arname   :: String
	, cellSize :: s
	, ar       :: t
	}

data Cell cell cell' t t' sz =
   Cell { cellName   :: String
	, offset     :: Int
	, updateCell :: ( (SZ cell' :==? sz) c
			, MaybeHandleIsOpen (Allocator hn hs) h c
			, MaybeHandleIsOpen (Allocator hn hs) h' c
			, MaybeFree (Allocator hn hs)
				    h
				    (Allocator hn hs')
				    c
			, MaybeAlloc (Allocator hn hs')
				     hn
				     (Allocator hn' hs'')
				     c
			)
		     => Ptr64 h t
			-> Ptr64 h' cell'
			-> PState l c
				  (MS rax rbx rcx rdx rsi rdi rbp rsp
                                      rflags rip r08 r09 r10 r11 r12
                                      r13 r14 r15 (Allocator hn hs) cmp)
                                  (MS rax rbx rcx rdx rsi rdi rbp rsp
                                      rflags rip r08 r09 r10 r11 r12
                                      r13 r14 r15
				      (Allocator hn' hs'') cmp)
				  y'
				  Composable
				  (Ptr64 hn t')
	, getCell    :: (MaybeHandleIsOpen alloc h c)
		     => Ptr64 h t
		     -> PState l c
			       (MS rax rbx rcx rdx rsi rdi rbp rsp
				   rflags rip r08 r09 r10 r11 r12
				   r13 r14 r15 alloc cmp)
			       (MS rax rbx rcx rdx rsi rdi rbp rsp
				   rflags rip r08 r09 r10 r11 r12
				   r13 r14 r15 alloc cmp)
			       y'
			       Composable
			       (Ptr64 h cell)
	}
     

data ArrayIngrediants =
    ArrayIngrediants { aiName	   :: String
		     , aiFields	   :: [(String, Int)]
		     , cellSizeInt :: Int
		     }

defineArray name cellSizeInt fields = defineArray' $
     ArrayIngrediants name
		      [ (fields !! i, i) | i <- [0 .. length fields - 1] ]
		      cellSizeInt
defineArray' ai =
     do typeDef <- defineType ai
	cellDefs <- mapM (uncurry $ defineCell ai) (aiFields ai)
	return (typeDef ++ concat cellDefs)

defineType ai =
     do let fieldNames = map fst (aiFields ai)
	return [ NewtypeD
		 []
		 (mkName $ aiName ai)
		 (map (PlainTV . mkName) fieldNames)
		 (NormalC (mkName $ aiName ai)
			  [ (NotStrict
			    , foldl (AppT) (ConT ''Array)
				    [ foldl (AppT) (TupleT (length fieldNames))
					    (map (VarT . mkName) fieldNames)
				    , mkT $ cellSizeInt ai
				    ]
			    )
			  ]
		 )
		 []
	       ]

defineCell ai name pos =
     do let offset = pos * cellSizeInt ai
	cell <- [| Cell { cellName   = name
			, offset     = offset
			, updateCell = \_ _ -> mixedReturn undefined
			, getCell    = \_ -> Core.return undefined
			}
		|]
	let cellType  = VarT $ mkName name
	    cellType' = VarT $ mkName $ name ++ "'"
	    replace _ _ [] = []
	    replace old new (a:as) = if old == a then new : (replace old new as)
						 else a : (replace old new as)
	    fieldNames = map fst (aiFields ai)
	    t  = foldl (AppT) (ConT $ mkName $ aiName ai)
			      (map (VarT . mkName) fieldNames)
	    t' = foldl (AppT) (ConT $ mkName $ aiName ai)
			      (replace cellType cellType'
				       (map (VarT . mkName) fieldNames))
	cellType <- return $ foldl (AppT) (ConT ''Cell)
				   [ cellType
				   , cellType'
				   , t
				   , t'
				   , (mkT $ cellSizeInt ai)
				   ]
	return [ SigD (mkName name)
		      (ForallT (map (PlainTV . mkName)
				    ( (name ++ "'") : fieldNames ))
			       []
			       cellType
		      )
	       , ValD (VarP $ mkName name) (NormalB cell) []
	       ]

