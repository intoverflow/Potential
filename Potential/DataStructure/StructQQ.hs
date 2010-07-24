module Potential.DataStructure.StructQQ (struct) where

import Prelude
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

import Potential.DataStructure.AbstractSyntax
import Potential.DataStructure.CommonParser
import Potential.DataStructure.CommonQQ

parseStruct fname line col s =
	let p = do pos <- getPosition
		   let pos'  = setSourceLine pos line
		       pos'' = setSourceColumn pos' col
		   setPosition pos''
		   structParser
	in case parse p fname s of
		Left err -> fail $ show err
		Right e -> return e

structParser =
     do whiteSpace
	structName   <- typeName
	string "where" >> whiteSpace
	structFields <- many1 $ do f <- parse_field
				   whiteSpace
				   return f
	eof
	-- now verify that the struct can be broken up into chunks of 64
	-- bits
	let ps = partition structFields [] []
	    valid = not $ any (\p -> 64 /= partition_size p) ps
	if valid
	  then return $ UserStruct structName ps
	  else fail $ "Fields for struct " ++ show structName ++
		      " fail to partition into 64-bit chunks!\n" ++
		      "  Partitions: \n" ++
		      (concat $ concat $
		       map (\p -> ["   size: " ++
				   show (partition_size p) ++
				   " bits\n"] ++
				  map pretty_field_show p) ps)
  where parse_field =
	     do f <- (try parse_field_const)
			<|> (try parse_field_reserved)
			<|> parse_field_var
		return f
	parse_field_const = 
	     do string "const"
		whiteSpace
		bits <- many1 bit
		return $ ConstField (fromIntegral $ length bits) bits
	parse_field_reserved =
	     do string "reserved"
		whiteSpace
		size <- integer
		return $ ReservedField size
	parse_field_var =
	     do name <- fieldName
		string "::"
		whiteSpace
		size <- integer
		return $ VarField name size
	partition_size p = sum $ map field_size p
	partition [] p' ps = filter (\p -> 0 /= partition_size p) (ps ++ [p'])
	partition (f:fs) p' ps
		| psize == 64 = partition fs [] (ps ++ [p'++[f]])
		| psize <  64 = partition fs (p' ++ [f]) ps
		| psize > 64  = ps ++ [p' ++ [f]]
	  where psize = partition_size (f:p')


struct = QuasiQuoter (parseStructExp parseStruct)
		     (parseStructPat parseStruct)

