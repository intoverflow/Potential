{-# LANGUAGE
	NoImplicitPrelude,
	EmptyDataDecls
	#-}
module Potential.IxMonad.IxMonad
	( IxMonad(..), IxMonadTrans(..)
	, Composable, Terminal
	) where

import Prelude( String )

data Composable
data Terminal

class IxMonad m where
  mixedReturn :: a -> m x y ct a
  return :: a -> m x x ct a
  return a = mixedReturn a
  (>>>=) :: m x y ct' a -> (a -> m y z ct b) -> m x z ct b
  (>>=)  :: m x y Composable a -> (a -> m y z ct b) -> m x z ct b
  m >>= f = m >>>= f
  (>>)   :: m x y Composable a -> m y z ct b  -> m x z ct b
  a >> b = a >>= (\_ -> b)
  fail :: String -> m x x Composable ()
  fail = fail

class IxMonadTrans t where
  lift :: IxMonad m => m x y ct a -> t m x y ct a

