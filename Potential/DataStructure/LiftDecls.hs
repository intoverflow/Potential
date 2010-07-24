{-# LANGUAGE
	TemplateHaskell,
	TypeSynonymInstances #-}
module Potential.DataStructure.LiftDecls where

import Prelude
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as THS
-- Requires:
--    th-lift on Hackage
--    by: Mathieu Boespflug <mboes@tweag.net>
import Language.Haskell.TH.Lift (deriveLift)

deriveLift ''Dec
deriveLift ''Kind
deriveLift ''FamFlavour
deriveLift ''Pragma
deriveLift ''Foreign
deriveLift ''FunDep
deriveLift ''Con
deriveLift ''TyVarBndr
deriveLift ''Pred
deriveLift ''Body
deriveLift ''Pat
deriveLift ''Clause
deriveLift ''Type
deriveLift ''Lit
deriveLift ''Exp
deriveLift ''Guard
deriveLift ''Strict
deriveLift ''Safety
deriveLift ''Callconv
deriveLift ''InlineSpec
deriveLift ''Stmt
deriveLift ''Range
deriveLift ''Match

instance THS.Lift Rational where
  lift a = undefined

