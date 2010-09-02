{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Standard Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the Potential Standard Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.DataStructure.CodeGenerator where

import Prelude
import qualified Language.Haskell.TH as TH
import Data.Char (toUpper)
import Data.Maybe (catMaybes)

import Language.Potential.THLiftDecls
import Language.Potential.DataStructure.AbstractSyntax
import Language.Potential.DataStructure.MetaData
import Language.Potential.Size (mkTypeNum, HasSZ(..), mkSize)
import Language.Potential.Pointer
	( newPtr64 , primPtrProj, primPtrInj , primFieldProj, primFieldInj )


-- |Given a type and an Int, defines an instance of HasSZ ascribing the given
-- size to the given type.
defineDataSize :: TH.Name -> Int -> TH.Q [TH.Dec]
defineDataSize typname sz =
	let typ = TH.ConT typname
	in return [ TH.InstanceD [] (TH.AppT (TH.ConT ''HasSZ) typ)
			[ TH.TySynInstD ''SZ [typ] (mkSize sz) ] ]


-- |Given a UserStruct, constructs a Template Haskell Type instance of the type
-- modeled by the given struct.  Type variables are *not* quantified.  They are
-- named according to their corresponding field labels.
typeFromStruct :: UserStruct
		-> (String -> String) -- ^ A function which transforms the names
				      -- of fields before they are made into
				      -- type variables.  'id' means no
				      -- transformation will be made.
		-> TH.Type
typeFromStruct us t =
     foldl TH.AppT (TH.ConT $ TH.mkName $ struct_name us)
	   (map (TH.VarT . TH.mkName . t) (varFieldNames $ allFields us))


-- |A handy function for making a string start with an upper-case character.
upperCase :: String -> String
upperCase (n:ns) = toUpper n : ns

-- |Given the name of a field, generates the name of its corresponding label.
labelName :: String -> String
labelName n = upperCase n


-- |Yields Template Haskell top-level declarations to define the given data
-- structure.
reifyStruct :: UserStruct -> TH.ExpQ
reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ reifyType
			, reifyFieldLabels ]
	[| return $ concat decls |]


-- |Defines a (Haskell-level) data definition for the given user structure.
-- Includes a free variable for every field label; if a field label is used
-- by multiple constructors, each instance will have the same free variable.
-- Also creates an instance of the NumConstructors relation to indicate how
-- many constructors the type has.
reifyType :: UserStruct -> TH.Q [TH.Dec]
reifyType us =
     do let name = TH.mkName $ struct_name us
	    type_vars = map (TH.PlainTV . TH.mkName)
			    (varFieldNames $ allFields us)
	    non_strict n = (TH.NotStrict, TH.VarT $ TH.mkName n)
	    mkConstr c = let vars = map non_strict (varFieldNames $ fields c)
			 in TH.NormalC (TH.mkName $ constr_name c) vars
	    def = TH.DataD [] name type_vars (map mkConstr $ constructors us) []
	    numconstr = countConstructors (typeFromStruct us id)
					  (length $ constructors us)
	return [def, numconstr]


-- |Given a type, creates an instance of the NumConstructors relation
-- for that type
countConstructors :: TH.Type -> Int -> TH.Dec
countConstructors typ n =
     let clss = foldl TH.AppT (TH.ConT ''NumConstructors)
				[typ, mkTypeNum n]
	 inst = TH.InstanceD [] clss []
     in inst


-- |Defines the (Haskell-level) field labels for all of the VarFields in this
-- type.  Also defines the IsFieldOf relation for each of these labels.  For
-- fields which are present in all constructors instances of the
-- AllConstructorsField relation will be defined.
reifyFieldLabels :: UserStruct -> TH.Q [TH.Dec]
reifyFieldLabels us =
     do let varFields = varFieldNames (allFields us)
	    struct_typ = typeFromStruct us id
	    mkLabel n = let lblname = TH.mkName $ labelName n
			    constr  = TH.NormalC lblname []
			in return $ TH.DataD [] lblname [] [constr] []
	    mkFun (name, val) = TH.FunD name [TH.Clause [] (TH.NormalB val) []]
	    mkIFORelation n =
	     do forgetMask'  <- [| \x y -> [ undefined ] |]
		isolateMask' <- [| \x y -> [ undefined ] |]
		bitOffset'   <- [| \x y -> [ undefined ] |]
		let lbl        = TH.ConT $ TH.mkName $ labelName n
		    field_typ  = TH.VarT $ TH.mkName n
		    clss = foldl TH.AppT (TH.ConT ''IsFieldOf)
					[struct_typ, lbl, field_typ]
		    defs = map mkFun
				[ ('forgetMask, forgetMask')
				, ('isolateMask, isolateMask')
				, ('bitOffset, bitOffset') ]
		return $ TH.InstanceD [] clss defs
	    mkACFRelation n =
		let constrHasF c = any (\f -> n == field_name f)
				       (filter isVarField $ fields c)
		    lbl        = TH.ConT $ TH.mkName $ labelName n
		    clss = foldl TH.AppT (TH.ConT ''AllConstructorsField)
					[ struct_typ, lbl ]
		in if all constrHasF (constructors us)
			then return $ Just (TH.InstanceD [] clss [])
			else return Nothing
	labelDecls   <- mapM mkLabel varFields
	ifoRelations <- mapM mkIFORelation varFields
	acfRelations <- mapM mkACFRelation varFields >>= return . catMaybes
	return $ labelDecls ++ ifoRelations ++ acfRelations

