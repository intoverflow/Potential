{-# LANGUAGE
	NoImplicitPrelude,
	EmptyDataDecls
	#-}
module Potential.IxMonad.IxMonad
	( IxFunctor(..), IxMonad(..), IxMonadTrans(..)
	, Composable, Terminal
	) where

import Prelude( String )

data Composable
data Terminal

class IxFunctor m where
  fmap :: (a -> b) -> m x y z ct a -> m x y z ct b

class IxFunctor m => IxMonad m where
  -- minimal interface
  mixedReturn :: a -> m x y z ct a
  (>>=)  :: m x y z Composable a -> (a -> m y z z' ct b) -> m x z z' ct b
  -- stuff for free
  return :: a -> m x x z ct a
  return a = mixedReturn a
  (>>)   :: m x y z Composable a -> m y z z' ct b  -> m x z z' ct b
  a >> b = a >>= (\_ -> b)
  fail :: String -> m x x x Composable a
  fail = fail

class IxMonadTrans t where
  lift :: IxMonad m => m x y z ct a -> t m x y z ct a

