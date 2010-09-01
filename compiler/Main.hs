{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Compiler.

    The Potential Compiler is free software: you can redistribute it and/or 
    modify it under the terms of the GNU General Public License as published 
    by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the Potential Compiler.  If not, see
    <http://www.gnu.org/licenses/>.
-}
module Main (main) where

import System.Console.CmdArgs

import PC.Compiler
import PC.Config

identification = "Potential Compiler 2010, (C) Tim Carstens 2010 http://potential-lang.org/"

main :: IO ()
main =
    (do conf <- cmdArgs identification [compileMode, checkMode, getTypeMode]
	if length (inFiles conf) == 1
	 then compiler conf
	 else do help <- cmdArgsHelp identification
				     [compileMode, checkMode, getTypeMode]
				     Text
		 putStrLn help
		 putStrLn "Exactly one MODULE must be supplied.")
    `catch` \e ->
    (do putStrLn identification
	print e)

