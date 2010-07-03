{-# LANGUAGE TemplateHaskell #-}
module Potential.DataStructure.CodeGenerator where

import qualified Language.Haskell.TH as TH

import Potential.DataStructure.LiftDecls
import Potential.DataStructure.AbstractSyntax
import Potential.Pointer (newPtr64)

reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ saveAST
			, reifyType
			, reifyPartialTypes
			-- , reifyAllocator
			]
	[| return $ concat decls |]

{-
    Our job:
    1. Define a new type for the given structure.
    2. Define a new type for each 64-bit partition of the given structure.
-}


reifyType :: UserStruct -> TH.Q [TH.Dec]
reifyType us =
     do let name = TH.mkName $ struct_name us
	    var_fields  = filter isVarField $ concat $ fields us
	    field_names = map (TH.mkName . field_name) var_fields
	    constructor = TH.NormalC name $
			  map (\n -> (TH.NotStrict, TH.VarT n)) field_names
	return [TH.DataD [] name (map TH.PlainTV field_names) [constructor] []]

reifyPartialTypes :: UserStruct -> TH.Q [TH.Dec]
reifyPartialTypes us =
     do decls <- mapM (reifyPartialType (struct_name us)) $
			zip (fields us) [0,8..]
	return $ concat decls

reifyPartialType :: String -> ([Field], Int) -> TH.Q [TH.Dec]
reifyPartialType name (fs, offset) =
     let name' = name ++ "_" ++ show offset
	 us' = UserStruct { struct_name = name'
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
     do let name        = TH.mkName $ "new" ++ struct_name us
	    var_fields  = filter isVarField $ concat $ fields us
	    field_names = map (TH.mkName . field_name) var_fields
	    new = foldl (TH.appE) (TH.varE name) (map TH.varE field_names)
	theFunction <- TH.appE [| newPtr64 |] new
	let theClause   = TH.Clause (map TH.VarP field_names)
				    (TH.NormalB theFunction)
				    []
	return [ TH.FunD name [theClause] ]


