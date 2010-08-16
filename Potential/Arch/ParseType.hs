module Potential.Arch.ParseType
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

