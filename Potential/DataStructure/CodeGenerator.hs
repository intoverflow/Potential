{-# LANGUAGE TemplateHaskell #-}
module Potential.DataStructure.CodeGenerator where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.LiftDecls
import Potential.DataStructure.AbstractSyntax
import Potential.Pointer (newPtr64, primPtrProj)

reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ saveAST
			, reifyType
			, reifyPartialTypes
			, reifyAllocator
			, reifyPartialProjectors
			]
	[| return $ concat decls |]

{- Helper functions -}
field_names us =
    let var_fields  = filter isVarField $ concat $ fields us
    in map (TH.mkName . field_name) var_fields

field_partial_names fs =
    let var_fields = filter isVarField fs
    in map (TH.mkName . field_name) var_fields

structType us var_names =
    let name   = TH.mkName $ struct_name us
	params = map TH.VarT var_names
    in foldl TH.AppT (TH.ConT name) params

structPartialName us offset = struct_name us ++ "_" ++ show offset

structPartialType us offset var_names =
    let name   = TH.mkName $ structPartialName us offset
	params = map TH.VarT var_names
    in foldl TH.AppT (TH.ConT name) params


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
reifyPartialTypes us =
     do decls <- mapM (reifyPartialType us) $
			zip (fields us) [0,8..]
	return $ concat decls

reifyPartialType :: UserStruct -> ([Field], Int) -> TH.Q [TH.Dec]
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

reifyAllocator :: UserStruct -> TH.Q [TH.Dec]
reifyAllocator us =
     do let alloc_name  = TH.mkName $ "new" ++ struct_name us
	    name        = TH.mkName $ struct_name us
	    new = foldl (TH.appE) (TH.conE name) (map TH.varE $ field_names us)
	theFunction <- TH.appE [| newPtr64 |] new
	let theClause   = TH.Clause (map TH.VarP $ field_names us)
				    (TH.NormalB theFunction)
				    []
	return [ TH.FunD alloc_name [theClause] ]


reifyPartialProjectors :: UserStruct -> TH.Q [TH.Dec]
reifyPartialProjectors us =
     do decls <- mapM (reifyPartialProjector us) $
			zip (fields us) [0,8..]
	return $ concat decls

reifyPartialProjector :: UserStruct -> ([Field], Int) -> TH.Q [TH.Dec]
reifyPartialProjector us (fs, offset) =
     do projectorUntyped <- [| \x -> undefined |]
	let proj_name = TH.mkName $ "proj_" ++ struct_name us ++ "_to_" ++
			show offset
	    domain    = structType us $ field_names us
	    range     = structPartialType us offset $ field_partial_names fs
	    signature = TH.ForallT (map TH.PlainTV $ field_names us)
				   []
				   (TH.AppT (TH.AppT TH.ArrowT domain) range)
	    projector = (TH.SigE projectorUntyped signature)
	theFunction  <- TH.appE [| \proj -> primPtrProj proj offset |]
				(return projector)
	return $ [ TH.ValD (TH.VarP proj_name) (TH.NormalB theFunction) [] ]

