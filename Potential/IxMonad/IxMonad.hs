{-# LANGUAGE
	NoImplicitPrelude,
	EmptyDataDecls
	#-}
module Potential.IxMonad.IxMonad
	( IxMonad(..)
	, Composable, Terminal
	) where

import Prelude( String )

data Composable
data Terminal

class IxMonad m where
  mixedReturn :: a -> m x y y' ct a
  return :: a -> m x x y' ct a
  return a = mixedReturn a
  (>>=)  :: m x y z Composable a -> (a -> m y z z' ct b) -> m x z z' ct b
  (>>)   :: m x y z Composable a ->       m y z z' ct b  -> m x z z' ct b
  a >> b = a >>= (\_ -> b)
  fail :: String -> m x x y' Composable ()
  fail = fail

