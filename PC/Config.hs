module PC.Config where

import System.Environment
import qualified Text.ParserCombinators.Parsec as P

import Control.Monad.State

parseConfig :: [String] -> State Config ()
parseConfig [] = return ()
parseConfig (a:as) =
     case a of
	"--check" -> do modify (\c -> c{ mode = Check })
			parseConfig as
	"--compile" -> do modify (\c -> c{ mode = Compile "asm" })
			  parseConfig as
	_ -> do addTarget a
		parseConfig as

addTarget :: FilePath -> State Config ()
addTarget t =
     do cfg <- get
	let target = Target { path = t }
	put $ cfg{ source = target : source cfg }

data Config =
    Config { source :: [Target]
	   , mode   :: Mode
	   }
  deriving Show

data Target = Target { path :: FilePath }
instance Show Target where show t = path t

data Mode =
    Compile { outdir :: FilePath }
  | Check
  deriving Show

getConfig :: IO Config
getConfig =
     do args <- getArgs
	return $ snd
	       $ runState (parseConfig args)
	       $ Config { source = [], mode = Check }

