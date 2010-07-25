module PC.Main where

import PC.Compiler
import PC.Config

main :: IO ()
main =
     do putStrLn "Potential compiler 2010 http://potential-lang.org/"
	k <- getConfig
	case k of
	  Right cfg -> compiler cfg
	  Left  ce  -> putStrLn $ "Configuration error: " ++ show ce

