module PC.Main where

import PC.Compiler
import PC.Config

main :: IO ()
main =
     do putStrLn "Potential compiler 2010 http://potential-lang.org/\n"
	config <- getConfig
	compile config

