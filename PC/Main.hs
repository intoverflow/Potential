module PC.Main where

import PC.Compiler
import PC.Config

main :: IO ()
main =
     do putStrLn "Potential compiler 2010 http://potential-lang.org/"
	config <- getConfig
	compile config

