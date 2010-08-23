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
module Language.Potential.Array.ArrayQQ (array) where

import Prelude
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

import Language.Potential.DataStructure.CommonParser
import Language.Potential.Array.AbstractSyntax
import Language.Potential.Array.CommonQQ

parseArray fname line col s =
     let p = do pos <- getPosition
		let pos'  = setSourceLine pos line
		    pos'' = setSourceColumn pos' col
		setPosition pos''
		arrayParser
     in case parse p fname s of
		Left err -> fail $ show err
		Right e -> return e

arrayParser =
     do whiteSpace
	arrayName <- typeName
	arrayFields <- many1 $ do f <- parse_field
				  whiteSpace
				  return f
	eof
	-- verify that the entries are ascending
	if (isStrictlyIncreasing arrayFields)
	  then return $ UserArray arrayName $ fillIn arrayFields
	  else fail $ "Array cells not in strictly increasing order.  Cells: "++
			show arrayFields
  where parse_field =
	     do p <- integer
		n <- identifier
		return $ if n == "reserved"
			    then ReservedField p
			    else VarField n p

array = QuasiQuoter (parseArrayExp parseArray)
                    (parseArrayPat parseArray)

