module Potential.Array.ArrayQQ (array) where

import Prelude
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

import Potential.DataStructure.CommonParser
import Potential.Array.AbstractSyntax
import Potential.Array.CommonQQ

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

