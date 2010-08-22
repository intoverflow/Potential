{-# LANGUAGE ScopedTypeVariables #-}
module Language.Potential.DataStructure.StructDiagramQQ (struct_diagram) where

import Prelude
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

import Data.List (sortBy)
import Data.Char (digitToInt)

import Language.Potential.DataStructure.AbstractSyntax
import Language.Potential.DataStructure.CommonParser
import Language.Potential.DataStructure.CommonQQ

parseStructDiagram fname line col s =
	let p = do pos <- getPosition
		   let pos'  = setSourceLine pos line
		       pos'' = setSourceColumn pos' col
		   setPosition pos''
		   structDiagramParser
	in case parse p fname s of
		Left err -> fail $ show err
		Right e -> return e

partitionSize :: [Field] -> Integer
partitionSize as = sum $ map field_size as

partition :: [Field] -> [[Field]]
partition fs = partition64 fs [] []

partition64 :: [Field] -> [Field] -> [[Field]] -> [[Field]]
partition64 [] p' ps = filter (not . null) $ ps ++ [p']
partition64 (f:fs) p' ps
    | partitionSize p' >= 64 = partition64 (f:fs) []          (ps ++ [p'])
    | otherwise              = partition64 fs     (p' ++ [f]) ps

structDiagramParser =
     do whiteSpace'
	structName   <- typeName
	whiteSpace'
	structFields <- many1 parse_diagram_field
	let structFields' = partition $
			    concat $
			    map fst $
			    sortBy (\(_, a :: Integer) (_, b :: Integer) ->
					compare a b)
				   structFields
	eof
	if any (\p -> partitionSize p /= 64) structFields'
	  then fail $ "Not all partitions are 64 bits in size.  Partitions: " ++
			show structFields'
	  else return $ UserStruct structName structFields'
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
		-- now we make sure the ranges are actually a partition.
		-- We do *not* enforce that the last bit is 31, or that the
		-- first bit is 0.  We only enforce that the numbers are
		-- decreasing.
		let ranges' = concat ranges
		if decreasing ranges'
			then return ranges
			else fail $ "Bit ranges are not strictly decreasing." ++
				    "  Numbers given: " ++ show ranges'
	  where decreasing [a] = True
		decreasing (a:b:ns) | a > b  = decreasing (b:ns)
				    | a <= b = False
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

