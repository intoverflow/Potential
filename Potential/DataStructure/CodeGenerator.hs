{-# LANGUAGE TemplateHaskell #-}
module Potential.DataStructure.CodeGenerator where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.LiftDecls
import Potential.DataStructure.AbstractSyntax
import Potential.Pointer ( newPtr64
			 , primPtrProj, primPtrInj
			 , primFieldProj, primFieldInj
			 )
import Potential.Size( (:==?), SZ, mkT )

import Data.List( mapAccumL )
import Data.Bits( shiftL, complement )

reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ saveAST
			, reifyType
			, reifyPartialTypes
			, reifyAllocator
			, reifyPartialProjectors
			, reifyPartialInjectors
			, reifyFieldProjectors
			, reifyFieldInjectors
			]
	[| return $ concat decls |]

{- Helper functions -}
field_names us =
    let var_fields = filter isVarField $ concat $ fields us
    in map (TH.mkName . field_name) var_fields

field_names_updated us fs =
    let var_fields = filter isVarField $ concat $ fields us
	names = map (TH.mkName . field_name) var_fields
	partials = zip (field_partial_names fs) (field_partial_names_updated fs)
    in map (\n -> maybe n (id) (lookup n partials)) names

field_partial_names fs =
    let var_fields = filter isVarField fs
    in map (TH.mkName . field_name) var_fields

field_partial_names_updated fs =
    let var_fields = filter isVarField fs
    in map (\f -> TH.mkName $ field_name f ++ "'") var_fields

field_name_updated f = TH.mkName $ field_name f ++ "'"

field_partial_names_updated_field fs f =
    let var_fields = filter isVarField fs
	names      = map (TH.mkName . field_name) var_fields
	name       = TH.mkName $ field_name f
	name'      = field_name_updated f
	updated    = [ (name, name') ]
    in map (\n -> maybe n (id) (lookup n updated)) names

structType us var_names =
    let name   = TH.mkName $ struct_name us
	params = map TH.VarT var_names
    in foldl TH.AppT (TH.ConT name) params

on_partials :: (UserStruct -> Partial -> TH.Q [TH.Dec])
		-> UserStruct -> TH.Q [TH.Dec]
on_partials reifier us =
     do decls <- mapM (reifier us) $ zip (fields us) [0,8..]
	return $ concat decls

structPartialName us offset = struct_name us ++ "_" ++ show offset

structPartialType us offset var_names =
    let name   = TH.mkName $ structPartialName us offset
	params = map TH.VarT var_names
    in foldl TH.AppT (TH.ConT name) params

on_fields :: (UserStruct -> Partial -> FieldWithBitOffset -> TH.Q [TH.Dec])
		-> UserStruct
		-> TH.Q [TH.Dec]
on_fields reifier us = on_partials reifier' us
  where reifier' us partial =
	     do let fs = prepareFields partial
		decls <- mapM (reifier us partial) fs
		return $ concat decls


-- Takes a Partial and computes the offset and masks for each
-- field.  Used to create partial/field injectors/projectors.
prepareFields :: Partial -> [FieldWithBitOffset]
prepareFields (fs, byte_offset) =
    let withBitOffset = snd $ mapAccumL prepareBitOffset 0 fs 
	varFieldsOnly = filter (\(f,_) -> isVarField f) withBitOffset
    in varFieldsOnly
  where prepareBitOffset bit_offset f =
	    let bit_offset' = bit_offset + field_size f
	    in (bit_offset', (f, bit_offset))

{-
    Our job:
    1. Define a new type for the given structure.
    2. Define a new type for each 64-bit partition of the given structure.
-}

reifyType :: UserStruct -> TH.Q [TH.Dec]
reifyType us =
     do let name = TH.mkName $ struct_name us
	    type_vars   = map TH.PlainTV $ field_names us
	    constructor = TH.NormalC name $
			  map (\n -> (TH.NotStrict, TH.VarT n)) (field_names us)
	return [TH.DataD [] name type_vars [constructor] []]

reifyPartialTypes :: UserStruct -> TH.Q [TH.Dec]
reifyPartialTypes us = on_partials reifyPartialType us

reifyPartialType :: UserStruct -> Partial -> TH.Q [TH.Dec]
reifyPartialType us (fs, offset) =
     let name = structPartialName us offset
	 us' = UserStruct { struct_name = name
			  , fields = [ fs ]
			  }
     in reifyType us'


{-
    Our job:
    1. Save the abstract syntax representation of the data structure
       (useful for debugging)
-}

saveAST :: UserStruct -> TH.Q [TH.Dec]
saveAST us =
     do let name = TH.mkName $ "ast_" ++ struct_name us
	us' <- [| us |]
	return [ TH.ValD (TH.VarP name) (TH.NormalB us') [] ]


{-
    Our job:
    1. Define an allocator
    2. Define accessors
    3. Define updaters
-}

-- This needs some work.  It's current approach is not safe, as it prescribes
-- types to uninitialized fields.
reifyAllocator :: UserStruct -> TH.Q [TH.Dec]
reifyAllocator us =
     do let alloc_name  = TH.mkName $ "new" ++ struct_name us
	    name        = TH.mkName $ struct_name us
	    new = foldl (TH.appE)
			(TH.conE name)
			(replicate (length $ field_names us) [| undefined |])
	theFunction <- TH.appE [| newPtr64 |] new
	let theClause   = TH.Clause [] -- (map TH.VarP $ field_names us)
				    (TH.NormalB theFunction)
				    []
	return [ TH.FunD alloc_name [theClause] ]


reifyPartialProjectors :: UserStruct -> TH.Q [TH.Dec]
reifyPartialProjectors us = on_partials reifyPartialProjector us

-- Creates mnemonics for projecting down from Type to Type_Offset
reifyPartialProjector :: UserStruct -> Partial -> TH.Q [TH.Dec]
reifyPartialProjector us (fs, offset) =
     do projectorUntyped <- [| \x -> undefined |]
	let proj_name = TH.mkName $ "proj_" ++ struct_name us ++ "_" ++
				    show offset
	    domain    = structType us $ field_names us
	    range     = structPartialType us offset $ field_partial_names fs
	    signature = TH.ForallT (map TH.PlainTV $ field_names us)
				   []
				   (TH.AppT (TH.AppT TH.ArrowT domain) range)
	    projector = TH.SigE projectorUntyped signature
	theFunction  <- TH.appE [| \proj -> primPtrProj proj offset |]
				(return projector)
	return $ [ TH.ValD (TH.VarP proj_name) (TH.NormalB theFunction) [] ]

reifyPartialInjectors :: UserStruct -> TH.Q [TH.Dec]
reifyPartialInjectors us = on_partials reifyPartialInjector us

-- Creates mnemonics for injecting up from Type_Offset to Type
reifyPartialInjector :: UserStruct -> Partial -> TH.Q [TH.Dec]
reifyPartialInjector us (fs, offset) =
     do injectorUntyped <- [| \x y -> undefined |]
	let inj_name = TH.mkName $ "inj_" ++ struct_name us ++ "_" ++
				   show offset
	    partialT  = structPartialType us offset $
				field_partial_names_updated fs
	    structT   = structType us $ field_names us
	    structT'  = structType us $ field_names_updated us fs
	    signature = TH.ForallT (map TH.PlainTV $
					field_names us ++
					field_partial_names_updated fs)
				   []
				   (TH.AppT (TH.AppT TH.ArrowT partialT)
				    (TH.AppT (TH.AppT TH.ArrowT structT)
				     structT'))
	    injector  = TH.SigE injectorUntyped signature
	theFunction  <- TH.appE [| \inj -> primPtrInj inj offset |]
				(return injector)
	return $ [ TH.ValD (TH.VarP inj_name) (TH.NormalB theFunction) [] ]


reifyFieldProjectors :: UserStruct -> TH.Q [TH.Dec]
reifyFieldProjectors us = on_fields reifyFieldProjector us

-- Create projector from partials to fields
reifyFieldProjector :: UserStruct
			-> Partial
			-> FieldWithBitOffset
			-> TH.Q [TH.Dec]
reifyFieldProjector us (fs, byte_offset) (f, bit_offset) =
     do projectorUntyped <- [| \x -> undefined |]
	let proj_name = TH.mkName $ "proj_" ++ struct_name us ++ "_" ++
				    show byte_offset ++ "_" ++ field_name f
	    type_proj_name = TH.mkName $ "proj_" ++ struct_name us ++ "_" ++
					 field_name f
	    domain1   = structPartialType us byte_offset $
				field_partial_names fs
	    domain2   = structType us $ field_names us
	    range     = TH.VarT $ TH.mkName $ field_name f
	    signature1 = TH.ForallT (map TH.PlainTV $ field_partial_names fs)
				    []
				    (TH.AppT (TH.AppT TH.ArrowT domain1) range)
	    signature2 = TH.ForallT (map TH.PlainTV $ field_names us)
				    []
				    (TH.AppT (TH.AppT TH.ArrowT domain2) range)
	    projector1 = TH.SigE projectorUntyped signature1
	    projector2 = TH.SigE projectorUntyped signature2
	theFunction  <- TH.appE [| \proj -> primFieldProj
						proj
						( (2^(field_size f)-1) `shiftL`
						  (fromIntegral bit_offset) )
						bit_offset |]
				(return projector1)
	return $ [ TH.ValD (TH.VarP proj_name) (TH.NormalB theFunction) []
		 , TH.ValD (TH.VarP type_proj_name) (TH.NormalB projector2) []
		 ]


reifyFieldInjectors :: UserStruct -> TH.Q [TH.Dec]
reifyFieldInjectors us = on_fields reifyFieldInjector us

-- Create injector from field to partial
-- TODO: impose class constraint on the size!
reifyFieldInjector :: UserStruct
			-> Partial
			-> FieldWithBitOffset
			-> TH.Q [TH.Dec]
reifyFieldInjector us (fs, byte_offset) (f, bit_offset) =
     do injectorUntyped <- [| \c x y -> undefined |]
	constraints     <- TH.newName "c"
	let inj_name = TH.mkName $ "inj_" ++ struct_name us ++ "_" ++
				   show byte_offset ++ "_" ++ field_name f
	    constraintsT = TH.VarT constraints
	    fieldT    = TH.VarT $ field_name_updated f
	    partialT  = structPartialType us byte_offset $
				field_partial_names fs
	    partialT' = structPartialType us byte_offset $
				field_partial_names_updated_field fs f
	    sizeC     = TH.ClassP ''(:==?)
				  [ TH.AppT (TH.ConT ''SZ) fieldT
				  , mkT $ field_size f
				  , constraintsT ]
	    signature = TH.ForallT (map TH.PlainTV $
					field_partial_names fs ++
					[ field_name_updated f
					, constraints ])
				   [sizeC]
				   (TH.AppT (TH.AppT TH.ArrowT constraintsT)
				    (TH.AppT (TH.AppT TH.ArrowT fieldT)
				     (TH.AppT (TH.AppT TH.ArrowT partialT)
				      partialT')))
	    injector  = TH.SigE injectorUntyped signature
	theFunction  <- TH.appE [| \inj -> primFieldInj
						inj
						( complement $
						  (2^(field_size f)-1) `shiftL`
						  (fromIntegral bit_offset) )
						bit_offset |]
				(return injector)
	return $ [ TH.ValD (TH.VarP inj_name) (TH.NormalB theFunction) [] ]


