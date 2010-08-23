{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
module Language.Potential.Arch.ParseType
	( parseType, typeName, typeVar, quantify
	, whiteSpace, identifier, parens, colon, commaSep
	) where

import Prelude

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)


lexer = P.makeTokenParser haskellDef
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
parens     = P.parens lexer
colon      = P.colon lexer
commaSep   = P.commaSep lexer
dot        = P.dot lexer

typeName   = do tname' <- upper
		tname'' <- identifier
		let tname = tname' : tname''
		whiteSpace
		return tname
typeVar    = P.identifier lexer
quantify   = do string "forall"
		whiteSpace
		vars <- many1 typeVar
		dot
		whiteSpace
		return vars

-- TODO: quantification needs to support kinded type vars and typeclasses
-- TODO: need to support other quasi-quoters other than the one we are
-- explicitly given
parseType parseModel' modelName = parseType'
  where parseType' = (parens parseType'') <|> parseType''
	parseType'' =  parseTypeName
		   <|> parseTypeVar
		   <|> parseForall
		   <|> parseModel
	parseTypeVar = typeVar >>= return . TH.VarT . TH.mkName
	parseTypeName =
	     do n  <- typeName >>= return . TH.ConT . TH.mkName
		as <- many parseType'
		return $ foldl TH.AppT n as
	parseForall =
	     do vars <- quantify
		t <- parseType'
		return $ TH.ForallT (map (TH.PlainTV . TH.mkName) vars) [] t
	parseModel =
	     do string "[$"
		string modelName
		char '|'
		t <- parseModel'
		string "|]"
		whiteSpace
		return t

