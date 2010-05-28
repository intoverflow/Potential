{-# LANGUAGE
	TypeFamilies,
	Rank2Types,
	ExistentialQuantification,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	TypeSynonymInstances,
	TemplateHaskell #-}
module Potential.BuildDataStructures 
	( defineDataSize
	, dataSize, dataSizeT
	, constField, reservedField, field
	, mkStruct, defineStruct
	, FieldModifier(..)
	) where

import Data.Word
import Data.Bits
import Numeric
import Language.Haskell.TH

import Potential.Core hiding (return, (>>), (>>=), fail)
import qualified Potential.Core as Core

import Potential.Size
import Potential.Bit
import Potential.Pointer


-- example: $(defineDataSize type size)
defineDataSize t n =
    return [ InstanceD
		[]
	    	(AppT (ConT ''HasSZ) (ConT t))
	    	[TySynInstD (mkName "SZ") [ConT t] (mkT n)]
	   ]

data ConstField c = ConstField Int c
instance HasIntSZ (ConstField c) where
  intSZ (ConstField s _) = s
data VarField = VarField { nm :: String, vfsz :: Int }
instance HasIntSZ VarField where
  intSZ = vfsz

data Field =
    VF VarField
  | forall c . CF (ConstField c)
instance HasIntSZ Field where
  intSZ (VF f) = intSZ f
  intSZ (CF c) = intSZ c

constField n =
     do varNames <- sequence $ replicate n $ newName "x"
	let vars = map VarP varNames
	    mkTup [v] = TupE [VarE v ]
	    mkTup (v:vs) = TupE [VarE v, mkTup vs]
	    tup = mkTup varNames
	getSize <- [| \t -> toInt $ sz t |]
	return $ LamE vars (AppE (ConE 'CF)
				 (AppE (AppE (ConE 'ConstField)
					     (AppE getSize tup))
				       tup))

reservedField n =
     do rf <- reservedField' n
	return $ AppE (ConE 'CF)
		      (AppE (AppE (ConE 'ConstField)
				  (LitE $ IntegerL n))
			    rf)
  where reservedField' 1 = [| (RB, ()) |]
	reservedField' n =
	     do f  <- reservedField' (n - 1)
		rb <- [| RB |]
		return $ TupE [rb, f]

field s name = VF $ VarField name s

getVarFields [] = []
getVarFields ((VF f) : fs) = f : getVarFields fs
getVarFields ( _ : fs) = getVarFields fs

mkStruct name fields = (name, fields)

data FieldModifier sz base base' shifted currentPackage forgottenPackage isolatedPackage updatedPackage currentStruct updatedStruct =
    FieldModifier { fname        :: String
		  , shiftBy      :: Int
		  , isolateMask  :: Word64
		  , forgetMask   :: Word64
		  , displacement :: Int
		  , shiftTyp     :: ((SZ base' :==? sz) c)
				 => base' -> PState l c x x y' Composable shifted
		  , shiftDownTyp :: isolatedPackage -> base
		  , forgetTyp    :: currentPackage -> forgottenPackage
		  , isolateTyp   :: currentPackage -> isolatedPackage
		  , orTyp        :: shifted -> forgottenPackage -> updatedPackage
		  , getStruct	 :: MaybeHandleIsOpen alloc h c
				 => Ptr64 h currentStruct
				 -> PState l c
					   (MS rax rbx rcx rdx rsi rdi rbp rsp
					       rflags rip r08 r09 r10 r11 r12
					       r13 r14 r15 alloc cmp)
					   (MS rax rbx rcx rdx rsi rdi rbp rsp
					       rflags rip r08 r09 r10 r11 r12
					       r13 r14 r15 alloc cmp)
					   y'
					   Composable
					   currentPackage
		  , setStruct	 :: ( MaybeHandleIsOpen (Allocator hn hs) h c
				    , MaybeFree  (Allocator hn hs)  h  (Allocator hn  hs') c
				    , MaybeAlloc (Allocator hn hs') hn (Allocator hn' hs'') c
				    )
				 => updatedPackage
				 -> Ptr64 h currentStruct
				 -> PState l c
					   (MS rax rbx rcx rdx rsi rdi rbp rsp
					       rflags rip r08 r09 r10 r11 r12
					       r13 r14 r15
					       (Allocator hn hs) cmp)
					   (MS rax rbx rcx rdx rsi rdi rbp rsp
					       rflags rip r08 r09 r10 r11 r12
					       r13 r14 r15
					       (Allocator hn' hs'') cmp)
					   y'
					   Composable
					   (Ptr64 hn updatedStruct)
		  }
instance Show (FieldModifier sz base base' shifted currentPackage forgottenPackage isolatedPackage updatedPackage currentStruct updatedStruct) where
  show bfm = "FieldModifier {fname = \""
	     ++ (fname bfm)
	     ++ "\", shiftBy = "
	     ++ show (shiftBy bfm)
	     ++ ", isolateMask = 0x"
	     ++ (showHex (isolateMask bfm) "")
	     ++ ", forgetMask = 0x"
	     ++ (showHex (forgetMask bfm) "")
	     ++ ", displacement = "
	     ++ show (displacement bfm)
	     ++ " }"

groupBy64 :: [Field] -> [Field] -> Q [[Field]]
groupBy64 a [] = return [a]
groupBy64 grp (f:fs)
	| sz == 64 = do grps <- groupBy64 [] (f:fs)
			return $ (reverse grp) : grps
	| sz > 64  = do report True ("Struct does not split into groups of 64-bits; hit trouble at field " ++ show (length (f:fs)) ++ " from the end.")
			return []
	| sz < 64  = groupBy64 (f:grp) fs
  where sz = sum $ map intSZ grp

defineStruct (name, fields) =
     do let sz = sum $ map intSZ fields
	-- verify that the struct size is a multiple of 64
	fail' (sz `mod` 64 /= 0)
	      ("Struct has " ++ show sz
	       ++ " bits, but a multiple of 64 bits are needed.")
	-- split the fields into their 64-bit packages
	groups	      <- groupBy64 [] fields
	-- make the data type definition for this struct
	typeDef       <- defineType name fields
	fieldTypeDefs <- mapM (uncurry defineType)
			      [(name ++ show i, groups !! i) |
				i <- [0 .. length groups - 1]]
	-- make the field accessors for this struct
	accessDefs    <- mapM (uncurry $ defineAccessors name fields)
			      [(i, groups !! i) | i <- [0 .. length groups - 1]]
	return (typeDef ++ concat fieldTypeDefs ++ concat accessDefs)
  where fail' b s = if b then report True s
			 else return ()

defineType name fields =
     do let sz = sum $ map intSZ fields
	    varFields = getVarFields fields
	    varNames  = map (mkName . nm) varFields
	    varSizes  = map (mkT . intSZ) varFields
	    vs = zip varNames varSizes
	    mkSizeAssumption (n, s) = EqualP (AppT (ConT ''SZ) (VarT n)) s
	    typ [a]     = AppT (TupleT $ length varFields) (VarT a)
	    typ (a:as)  = AppT (typ as) (VarT a)
	    inst [a]    = AppT (ConT $ mkName name) (VarT a)
	    inst (a:as) = AppT (inst as) (VarT a)
	return $ [ NewtypeD	-- defines the new type
		   [] -- might not need this (map mkSizeAssumption vs)
		   (mkName name)
		   (map PlainTV varNames)
		   (NormalC (mkName name)
			    [(NotStrict, typ $ reverse varNames)]
		   )
		   []
	         , InstanceD	-- defines the size of the new type
		   []
		   (AppT (ConT ''HasSZ) (inst $ reverse varNames))
		   [ TySynInstD (mkName "SZ")
				[inst $ reverse varNames]
				(mkT sz)
		   ]
	         ]

defineAccessors nameStruct fields displacement group = defineAccessors' group 0
  where defineAccessors' [] _ = return []
	defineAccessors' (CF f:fs) shift = defineAccessors' fs (shift + intSZ f)
	defineAccessors' (VF f:fs) shift =
	     do accessorDef  <- defineAccessor f shift
		accessorDefs <- defineAccessors' fs (shift + intSZ f)
		return $ accessorDef ++ accessorDefs
	defineAccessor f shift =
	     do let fname       = nm f
		    fieldSize   = fromIntegral $ intSZ f :: Integer
		fm <- [| let isolateMask = (2^fieldSize - 1) `shiftL` shift
			     forgetMask  = complement isolateMask
			 in FieldModifier
				{ fname        = fname
				, shiftBy      = shift
				, isolateMask  = isolateMask
				, forgetMask   = forgetMask
					-- number of bytes into the struct
				, displacement = displacement * 8
				, shiftTyp     = \_ -> Core.return undefined
				, shiftDownTyp = \_ -> undefined
				, forgetTyp    = \_ -> undefined
				, isolateTyp   = \_ -> undefined
				, orTyp        = \_ _ -> undefined
				, getStruct    = \_ -> Core.return undefined
				, setStruct    = \_ _ -> mixedReturn undefined
				}
		      |]
		let varFields = getVarFields fields
		    varNames   = map (mkName . nm) varFields
		    varNames'  = map (\f -> mkName (nm f ++ "'")) varFields
		    varNames'' = map (\f -> mkName (nm f ++ "''")) varFields
		    varFieldsGroup = getVarFields group
		    varNamesGroup   = map (mkName . nm) varFieldsGroup
		    varNamesGroup'  = map (\f -> mkName (nm f ++ "'"))
					  varFieldsGroup
		    varNamesGroup'' = map (\f -> mkName (nm f ++ "''"))
					  varFieldsGroup
		    fname'    = nm f ++ "'"
		    fname''   = nm f ++ "''"
		    nameGroup = nameStruct ++ show displacement
		    inst n vs = foldl (AppT) (ConT $ mkName n) (map VarT vs)
		    shifted   = inst nameGroup varNamesGroup'
		    replace old new []     = []
		    replace old new (a:as) = if a == old
						then (new : replace old new as)
						else (a : replace old new as)
		    currentPackage   = inst nameGroup varNamesGroup
		    forgottenPackage = let vars = replace (mkName fname)
							  (mkName fname'')
							  varNamesGroup
				       in inst nameGroup vars
		    isolatedPackage  = let vars = replace (mkName fname'')
							  (mkName fname)
							  varNamesGroup''
				       in inst nameGroup vars
		    updatedPackage   = let vars = replace (mkName fname)
							  (mkName fname')
							  varNamesGroup
				       in inst nameGroup vars
		    currentStruct    = inst nameStruct varNames
		    updatedStruct    = let vars = replace (mkName fname)
							  (mkName fname')
							  varNames
				       in inst nameStruct vars
		fmTyp <- return $ foldl (AppT)
					(ConT ''FieldModifier)
					[ mkT $ intSZ f		-- sz
					, VarT $ mkName fname	-- base
					, VarT $ mkName fname'	-- base'
					, shifted
					, currentPackage
					, forgottenPackage
					, isolatedPackage
					, updatedPackage
					, currentStruct
					, updatedStruct
					]
		proj <- [| \_ -> undefined |]
		projTyp <- return $ foldl (AppT)
					  (ArrowT)
					  [ inst nameStruct varNames
					  , VarT $ mkName fname
					  ]
		return [ ValD (VarP $ mkName fname) (NormalB fm) []
		       , SigD (mkName fname)
			      (ForallT (map PlainTV (varNames ++
						     varNames' ++
						     varNames''))
				       [] fmTyp)
		       , ValD (VarP $ mkName $ "proj_" ++ fname)
			      (NormalB proj) []
		       , SigD (mkName $ "proj_" ++ fname)
			      (ForallT (map PlainTV varNames) [] projTyp)
		       ]

