{-# LANGUAGE ScopedTypeVariables #-}
module Potential.DataStructure.StructDiagramQQ (struct_diagram) where

import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

import Data.List
import Data.Char (digitToInt)

import Potential.DataStructure.AbstractSyntax
import Potential.DataStructure.CommonParser
import Potential.DataStructure.CommonQQ

parseStructDiagram fname line col s =
	let p = do pos <- getPosition
		   let pos'  = setSourceLine pos line
		       pos'' = setSourceColumn pos' col
		   setPosition pos''
		   structDiagramParser
	in case parse p fname s of
		Left err -> fail $ show err
		Right e -> return e

structDiagramParser =
     do whiteSpace'
	structName   <- typeName
	whiteSpace'
	structFields <- many1 parse_diagram_field
	let structFields' = map fst $
			    sortBy (\(_,a :: Integer) (_,b :: Integer) ->
					compare a b)
				   structFields
	    structFields'' = pairup structFields'
	    pairup [] = []
	    pairup (a:b:cs) = (a ++ b) : pairup cs
	eof
	return $ UserStruct structName structFields''
  where whiteSpace' = try parse_diagram_comment <|> whiteSpace
	bitpos = do digits <- many1 digit
		    return $ fromIntegral $ convert 0 digits
	  where convert n [] = n
		convert n (d:ds) = convert (10*n + digitToInt d) ds
	parse_diagram_comment =
	     do whiteSpace
		char '('
		manyTill anyChar (try $ char '\n')
		whiteSpace'
	parse_diagram_field =
	     do ranges <- parse_top
		(fs, byte_offset) <- parse_middle ranges
		parse_bottom
		whiteSpace'
		return (fs, byte_offset)
	parse_top =
	     do char ('|')
		(ranges :: [[Integer]]) <- sepEndBy1 (try parse_top') (char '|')
		whiteSpace'
		-- now we make sure the ranges are actually a partition
		-- of the interval [0, 31].
		let ranges' = concat ranges
		if decreasing_partition ranges'
			then return ranges
			else fail $ "Bit ranges do not partition [0, 31].\n" ++
				    "Numbers given: " ++ show ranges'
	  where decreasing_partition (31:ns) = decreasing_partition' ns
		decreasing_partition _ = False
		decreasing_partition' [] = False
		decreasing_partition' [0] = True
		decreasing_partition' [a, 0] | a > 0 = True
					     | otherwise = False
		decreasing_partition' (a:b:ns)
			| a > b = decreasing_partition' (b:ns)
			| otherwise = False
		parse_top' =
		     do many (char '-')
			ulimit <- bitpos >>= \l -> return [l]
			many (char '-')
			llimit <- (try $ bitpos >>= \l -> return [l])
				  <|> (return [])
			many (char '-')
			case (ulimit, llimit) of
			  ([u], [l]) -> if u > l
					  then return [u,l]
					  else fail $ show u ++ " <= " ++ show l
			  ([u], [])  -> return [u]
	parse_middle rs = do char '|'
			     parse_middle' rs []
	  where parse_middle' [] fs =
		     do whiteSpace
			byte_offset <- integer
			whiteSpace'
			return (fs, byte_offset)
		parse_middle' (r:rs) fs =
		     do whiteSpace
			f <- parse_field r (span r)
			char '|'
			parse_middle' rs (f:fs)
		  where span [a] = 1
			span [a,b] = a-b+1
	parse_field r size =
		(try $ do string "reserved"
			  whiteSpace
			  return $ ReservedField size)
	    <|> (try $ do name <- fieldName
			  whiteSpace
			  return $ VarField name size)
	    <|> (do bits <- many1 bit
		    whiteSpace
		    let bl = fromIntegral $ length bits
		    if size == bl
			then return $ ConstField size bits
			else fail $ "Field is defined to be " ++ show size ++
				    " bits, but there are " ++ show bl ++
				    " bits in this field (location " ++
				    show r ++ ")")
	parse_bottom =
	     do char '|'
		many (char '-')
		char '|'
		whiteSpace'

struct_diagram = QuasiQuoter (parseStructExp parseStructDiagram)
			     (parseStructPat parseStructDiagram)

