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
module Language.Potential.DataStructure.CommonParser
	( whiteSpace, parens
	, identifier, integer, float
	, typeName, bit, fieldName
	) where

import Prelude
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Language.Potential.DataStructure.AbstractSyntax

lexer = P.makeTokenParser haskellDef
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
integer    = P.integer lexer
float      = P.float lexer
parens     = P.parens lexer
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
		"reserved" -> fail "Variable fields cannot be named `reserved'"
		"const"    -> fail "Variable fields cannot be named `const'"
		_ -> return name


