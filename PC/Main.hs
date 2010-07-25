module PC.Main (main) where

import System.Console.CmdArgs

import PC.Compiler
import PC.Config

identification = "Potential Compiler 2010, (C) Tim Carstens 2010 http://potential-lang.org/"

main :: IO ()
main =
    (do conf <- cmdArgs identification [compileMode, checkMode]
	if length (inFiles conf) == 1
	 then compiler conf
	 else do help <- cmdArgsHelp identification
				     [compileMode, checkMode]
				     Text
		 putStrLn help
		 putStrLn "Exactly one MODULE must be supplied.")
    `catch` \e ->
    (do putStrLn identification
	print e)

