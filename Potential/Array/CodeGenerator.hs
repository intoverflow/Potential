{-# LANGUAGE TemplateHaskell #-}
module Potential.Array.CodeGenerator (reifyArray) where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.LiftDecls
import Potential.Array.AbstractSyntax
import Potential.Size (dataSize)
import Potential.Pointer (newPtr64, primArrayProj, primArrayInj)

import qualified Potential.DataStructure.AbstractSyntax as DAS
import qualified Potential.DataStructure.CodeGenerator as DCG

reifyArray :: DAS.UserStruct -> UserArray -> TH.Q [TH.Dec]
reifyArray astCell ua =
     do decls <- mapM (\f -> f ua)
			[ saveAST
			, reifyType
			, reifyAllocator
			, reifyProjectors astCell
			, reifyInjectors astCell
			]
        return $ concat decls

saveAST :: UserArray -> TH.Q [TH.Dec]
saveAST us =
     do let name = TH.mkName $ "ast_" ++ array_name us
	us' <- [| us |]
	return [ TH.ValD (TH.VarP name) (TH.NormalB us') [] ]


{-- Helper functions --}

field_names ua =
    let var_fields = filter isVarField $ fields ua
    in map (TH.mkName . field_name) var_fields

arrayType ua var_names =
    let name   = TH.mkName $ array_name ua
	params = map TH.VarT var_names
    in foldl TH.AppT (TH.ConT name) params

arrayTypeWithCell ua var_names f t =
    let name   = TH.mkName $ array_name ua
	params = map (\n -> if (TH.mkName $ field_name f) == n
				then t
				else TH.VarT n)
		     var_names
    in foldl TH.AppT (TH.ConT name) params

cellType astCell =
     do let name = TH.mkName $ DAS.struct_name astCell
	info <- TH.reify name
	case info of
	  TH.TyConI (TH.DataD _ _ var_fields' _ _) ->
	     do let var_fields = map (\(TH.PlainTV n) -> TH.VarT n) var_fields'
		return $ TH.ForallT var_fields' [] $
			 (foldl TH.AppT (TH.ConT name) var_fields)
	  _ -> fail $ "Expected type.  Given: " ++ show name

genericCellSize astCell =
	TH.appE [| \t -> (dataSize t `div` 8) |]
		(TH.sigE [| undefined |] (cellType astCell))

on_var_fields :: (Field -> TH.Q [TH.Dec]) -> UserArray -> TH.Q [TH.Dec]
on_var_fields reifier ua =
     do let var_fields = filter isVarField $ fields ua
	decls <- mapM reifier var_fields
	return $ concat decls

{-- Define the data type --}
reifyType :: UserArray -> TH.Q [TH.Dec]
reifyType ua =
     do let name = TH.mkName $ array_name ua
	    fs   = field_names ua
	    type_vars = map TH.PlainTV fs
	    constructor = TH.NormalC name $
			  map (\n -> (TH.NotStrict, TH.VarT n)) fs
	return [TH.DataD [] name type_vars [constructor] []]

{-- Allocators --}
reifyAllocator :: UserArray -> TH.Q [TH.Dec]
reifyAllocator ua =
     do let alloc_name = TH.mkName $ "new" ++ array_name ua
	    name       = TH.mkName $ array_name ua
	    new = foldl TH.appE
			(TH.conE name)
			(replicate (length $ field_names ua) [| undefined |])
	theFunction <- TH.appE [| newPtr64 |] new
	let theClause  = TH.Clause []
				   (TH.NormalB theFunction)
				   []
	return [ TH.FunD alloc_name [theClause] ]

{-- Projectors from the array down to cells --}
reifyProjectors :: DAS.UserStruct -> UserArray -> TH.Q [TH.Dec]
reifyProjectors astCell ua = on_var_fields (reifyProjector astCell ua) ua

reifyProjector :: DAS.UserStruct -> UserArray -> Field -> TH.Q [TH.Dec]
reifyProjector astCell ua f =
     do projectorUntyped   <- [| \x -> undefined |]
	let proj_name = TH.mkName $ "proj_" ++ array_name ua ++ "_" ++
				    field_name f
	    domain    = arrayType ua $ field_names ua
	    range     = TH.VarT $ TH.mkName $ field_name f
	    signature = TH.ForallT (map TH.PlainTV $ field_names ua)
				   []
				   (TH.AppT (TH.AppT TH.ArrowT domain) range)
	    projector = TH.SigE projectorUntyped signature
	offset       <- TH.appE [| \sz -> sz * field_pos f |]
				(genericCellSize astCell)
	theFunction  <- foldl TH.appE [| primArrayProj |]
				[ return projector, return offset ]
	return $ [ TH.ValD (TH.VarP proj_name) (TH.NormalB theFunction) [] ]

{-- Injectors from the cell's partials to the array --}
reifyInjectors :: DAS.UserStruct -> UserArray -> TH.Q [TH.Dec]
reifyInjectors astCell ua =
   on_var_fields (\f -> DCG.on_partials (reifyInjector f astCell ua) astCell) ua

reifyInjector :: Field -> DAS.UserStruct -> UserArray
		    -> DAS.Partial -> TH.Q [TH.Dec]
reifyInjector f astCell ua (fs, poffset) =
     do injectorUntyped <- [| \x y -> undefined |]
	let inj_name = TH.mkName $ "inj_" ++ field_name f ++ "_" ++
				   DAS.struct_name astCell ++ "_" ++
				   show poffset
	    partialT = DCG.structPartialType astCell poffset $
			DCG.field_partial_names_updated fs
	    cellT    = DCG.structType astCell $
			DCG.field_names astCell
	    cellT'   = DCG.structType astCell $
			DCG.field_names_updated astCell fs
	    arrayT   = arrayTypeWithCell ua (field_names ua) f cellT
	    arrayT'  = arrayTypeWithCell ua (field_names ua) f cellT'
	    signature = TH.ForallT (map TH.PlainTV $
					field_names ua ++
					DCG.field_names astCell ++
					DCG.field_partial_names_updated fs)
				   []
				   (TH.AppT (TH.AppT TH.ArrowT partialT)
				    (TH.AppT (TH.AppT TH.ArrowT arrayT)
				     arrayT'))
	    injector = TH.SigE injectorUntyped signature
	    offset   = field_pos f
	theFunction <- TH.appE [| \inj -> primArrayInj inj offset |]
				(return injector)
	return [ TH.ValD (TH.VarP inj_name) (TH.NormalB theFunction) [] ]

