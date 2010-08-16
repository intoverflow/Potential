{-# LANGUAGE TemplateHaskell #-}
module Potential.Arch.CommonQQ (parseModel , parseExp, parsePat) where

import Prelude

import Text.ParserCombinators.Parsec

import Data.Generics.Aliases (extQ)
import qualified Language.Haskell.TH as TH
import Potential.THLiftDecls
import Language.Haskell.TH.Quote (dataToExpQ, dataToPatQ)

antiE :: TH.Type -> Maybe TH.ExpQ
antiE typ = Just [| return typ |]

parseExp parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToExpQ (const Nothing `extQ` antiE) parsed

parsePat parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToPatQ (const Nothing) parsed

parseModel modelParser fname line col s =
     let p = do pos <- getPosition
		let pos'  = setSourceLine pos line
		    pos'' = setSourceColumn pos' col
		setPosition pos''
		modelParser
     in case parse p fname s of
		Left err -> fail $ show err
		Right e  -> return e


