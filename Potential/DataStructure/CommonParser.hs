module Potential.DataStructure.CommonParser
	( whiteSpace
	, identifier
	, integer
	, typeName
	, bit
	, fieldName
	) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Potential.DataStructure.AbstractSyntax

lexer = P.makeTokenParser haskellDef
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
integer    = P.integer lexer
typeName   = do structName' <- upper
		structName'' <- identifier
		let structName = structName' : structName''
		whiteSpace
		return structName
bit = (try $ char '0' >> return ConstBit0)
	<|> (try $ char '1' >> return ConstBit1)
fieldName =
     do name <- identifier
	-- fail if name == "reserved" or name == "const"
	case name of
		"reserved" -> fail  "Variable fields cannot be named `reserved'"
		"const"    -> fail "Variable fields cannot be named `const'"
		_ -> return name


