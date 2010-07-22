module PC.Main where

import PC.Compiler
import PC.Config

main :: IO ()
main =
     do putStrLn "Potential compiler"
	putStrLn "2010 http://potential-lang.org/\n"
	config <- getConfig
	compile config

