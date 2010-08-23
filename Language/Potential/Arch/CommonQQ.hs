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
{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.Arch.CommonQQ (parseModel , parseExp, parsePat) where

import Prelude

import Text.ParserCombinators.Parsec

import Data.Generics.Aliases (extQ)
import qualified Language.Haskell.TH as TH
import Language.Potential.THLiftDecls
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


