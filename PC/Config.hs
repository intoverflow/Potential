module PC.Config where

import System.Environment
import Text.ParserCombinators.Parsec

parseConfig :: [String] -> Config
parseConfig args = Config { source = args }

data Config =
    Config { source :: [FilePath]
	   }
  deriving Show

getConfig :: IO Config
getConfig =
     do args <- getArgs
	return $ parseConfig args

