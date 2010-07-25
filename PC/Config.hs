module PC.Config where

import System.Environment
import qualified Text.ParserCombinators.Parsec as P

import Control.Monad.State
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

data Config =
    Config { target :: Target
	   , mode   :: Mode
	   }
  deriving Show

data MaybeConfig =
    MaybeConfig { mtarget :: Maybe Target
		, mmode   :: Maybe Mode
		, moutdir :: Maybe FilePath
		}
  deriving Show

data Target = Target { path :: FilePath }
instance Show Target where show t = show $ path t

data Mode =
    Compile { outdir :: FilePath }
  | Check
  deriving (Show, Eq)

data ConfigError =
    ModeAlreadySet
  | TargetAlreadySet
  | OutDirAlreadySet
  | OutDirInCheckMode
  | ConfigIncomplete MaybeConfig
  deriving Show

parseConfig :: [String] -> State MaybeConfig (Either ConfigError Config)
parseConfig [] =
     do mcfg <- get
	case (mtarget mcfg, mmode mcfg) of
	  (Just target, Just mode) ->
	    let config = Config { target = target
				, mode   = mode }
	    in maybe (return $ Right config)
		     (\outdir -> return $
			     if mode == Check
				then Left OutDirInCheckMode
				else Right config{ mode = Compile outdir })
		     (moutdir mcfg)
	  _ -> return $ Left $ ConfigIncomplete mcfg
parseConfig (a:as) =
     do me <- case a of
	    "--check"   -> setMode Check
	    "--compile" -> setMode $ Compile "asm"
	    _ | isPrefixOf "--outdir=" a -> setOutdir (fromJust $ stripPrefix "--outdir=" a)
	      | otherwise -> setTarget a
	case me of
	  Nothing -> parseConfig as
	  Just e  -> return $ Left e

setMode :: Mode -> State MaybeConfig (Maybe ConfigError)
setMode m =
     do cfg <- get
	case (mmode cfg) of
	  Nothing -> do put cfg{ mmode = Just m }
			return Nothing
	  Just _  -> return $ Just ModeAlreadySet

setOutdir :: FilePath -> State MaybeConfig (Maybe ConfigError)
setOutdir od =
     do cfg <- get
	case (moutdir cfg) of
	  Nothing -> do put cfg{ moutdir = Just od }
			return Nothing
	  Just _  -> return $ Just OutDirAlreadySet

setTarget :: FilePath -> State MaybeConfig (Maybe ConfigError)
setTarget fp =
     do cfg <- get
	case (mtarget cfg) of
	  Nothing -> do put cfg{ mtarget = Just $ Target fp }
			return Nothing
	  Just _  -> return $ Just TargetAlreadySet

getConfig :: IO (Either ConfigError Config)
getConfig =
     do args <- getArgs
	return $ fst
	       $ runState (parseConfig args)
	       $ MaybeConfig { mtarget = Nothing
			     , moutdir = Nothing
			     , mmode = Nothing }

