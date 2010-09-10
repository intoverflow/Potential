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
import qualified Language.Haskell.TH.Syntax as THS
import Data.Char (toUpper)
import Data.Maybe (catMaybes)

import Language.Potential.THLiftDecls
import Language.Potential.DataStructure.AbstractSyntax
import Language.Potential.DataStructure.MetaData
import Language.Potential.Size (mkTypeNum, HasSZ(..), mkSize)


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
fieldLabelName :: String -> String
fieldLabelName n = upperCase n

-- Given the name fo a constructor, generates the name of its corresponding
-- label.
constructorLabelName :: String -> String
constructorLabelName n = 'C' : n


-- Given the name of a label, creates a data definition for the corresponding
-- label.  Also creates the corresponding (derived) instance for Show and Lift.
reifyLabel :: String -> TH.Q [TH.Dec]
reifyLabel n =
     do let lblname = TH.mkName $ fieldLabelName n
	    constr  = TH.NormalC lblname []
	    datad   = TH.DataD [] lblname [] [constr] [''Show]
	    lbl   = TH.ConT lblname
	    clss  = TH.AppT (TH.ConT ''THS.Lift) lbl
	lifter <- [| TH.ConE lblname |]
	lifted <- TH.appE [| return |] [| TH.ConE lblname |]
	let lift' = TH.FunD ('THS.lift)
			    [TH.Clause [TH.WildP] (TH.NormalB lifted) []]
	    lifti = TH.InstanceD [] clss [lift']
	return [datad, lifti]

-- |Yields Template Haskell top-level declarations to define the given data
-- structure.
reifyStruct :: UserStruct -> TH.ExpQ
reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ reifyType
			, reifyConstructorLabels
			, reifyFieldLabels
			, reifyFieldRelations
			, reifyAllConstructorsRelations ]
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
	TH.TySynInstD ''NumConstructors [typ] (mkTypeNum n)


-- |Defines the (Haskell-level) constructor labels for all of the constructors
-- of teh given structure.
reifyConstructorLabels :: UserStruct -> TH.Q [TH.Dec]
reifyConstructorLabels us =
     do let constrNames = map (constructorLabelName . constr_name)
			      (constructors us)
	mapM reifyLabel constrNames >>= return . concat


-- |Defines the (Haskell-level) field labels for all of the VarFields in this
-- type.
reifyFieldLabels :: UserStruct -> TH.Q [TH.Dec]
reifyFieldLabels us =
     do let varFields = varFieldNames (allFields us)
	mapM reifyLabel varFields >>= return . concat


-- |Defines the IsField, ProjField, and DeepAccess relations for all of the
-- fields for the given structure.
reifyFieldRelations :: UserStruct -> TH.Q [TH.Dec]
reifyFieldRelations us =
     do constr <- TH.newName "constr"
	let oneConstructor = length (constructors us) == 1
	    structTyp = let structTyp' = typeFromStruct us id
			in if oneConstructor
				then TH.AppT (TH.ConT ''NotConstructed)
					structTyp'
				else foldl TH.AppT (TH.ConT ''Constructed)
					[ TH.VarT constr
					, structTyp' ]
	    mkFun (name, val) = TH.FunD name [TH.Clause [] (TH.NormalB val) []]
	    mkIFRelation n =
	     do let fa = getFieldAccess us n
		access' <- if oneConstructor
			     then [| \_ _ -> OneConstr
					{ access_params = (snd . head) fa
					, accessor_name = n
					} |]
			     else [| \st fl -> ManyConstr
				      { strategies =
					    map (\(c, a) -> (rep_by c, a)) fa
				      , accessor_name = n
				      } |]
		let lbl      = TH.ConT $ TH.mkName $ fieldLabelName n
		    fieldTyp = TH.VarT $ TH.mkName n
		    assuming = if oneConstructor
				then []
				else [ TH.ClassP ''Show [TH.VarT constr]
				     , TH.ClassP ''THS.Lift [TH.VarT constr] ]
		    clss = foldl TH.AppT (TH.ConT ''IsField) [structTyp, lbl]
		    defs = map mkFun [ ('access, access') ]
		return $ TH.InstanceD [] clss defs
	    mkPFRelation n =
	     do let lbl      = TH.ConT $ TH.mkName $ fieldLabelName n
		    fieldTyp = TH.VarT $ TH.mkName n
		    clss     = foldl TH.AppT (TH.ConT ''ProjField)
				[structTyp, lbl]
		    defs = [TH.TySynInstD ''FieldType [structTyp, lbl] fieldTyp]
		return $ TH.InstanceD [] clss defs
	    mkDARelation n =
	     do deepAccess' <- [| \st l -> [ accessWithConstr st l ] |]
		let lbl  = TH.ConT $ TH.mkName $ fieldLabelName n
		    fieldTyp = TH.VarT $ TH.mkName n
		    assuming = [TH.ClassP ''GetConstructorData
				 [structTyp, foldl TH.AppT (TH.ConT ''FieldType)
						 [structTyp, lbl]]
			       ,TH.ClassP ''ProjField
				 [structTyp, lbl] ]
		    clss = foldl TH.AppT (TH.ConT ''DeepAccess)
				[structTyp, lbl]
		    defs = map mkFun [ ('deepAccess, deepAccess') ]
		return $ TH.InstanceD assuming clss defs
	    varFields = varFieldNames (allFields us)
	ifrs <- mapM mkIFRelation varFields
	pfrs <- mapM mkPFRelation varFields
	dars <- mapM mkDARelation varFields
	return $ ifrs ++ pfrs ++ dars


-- |Defines the AllConstructorsField relation for all applicable fields of the
-- given structure.
reifyAllConstructorsRelations :: UserStruct -> TH.Q [TH.Dec]
reifyAllConstructorsRelations us =
     do let varFields = varFieldNames (allFields us)
	    structTyp = typeFromStruct us id
	    mkACFRelation n =
		let constrHasF c = any (\f -> n == field_name f)
				       (filter isVarField $ fields c)
		    lbl        = TH.ConT $ TH.mkName $ fieldLabelName n
		    clss = foldl TH.AppT (TH.ConT ''AllConstructorsField)
					[ structTyp, lbl ]
		in if all constrHasF (constructors us)
			then return $ Just (TH.InstanceD [] clss [])
			else return Nothing
	mapM mkACFRelation varFields >>= return . catMaybes


