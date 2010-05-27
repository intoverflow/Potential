{-# LANGUAGE
	EmptyDataDecls
	#-}
module Potential.Monad.IxMonad
	( IxMonad(..)
	, IxMonadTrans(..)
	, Composable, Terminal
	) where

data Composable
data Terminal

class IxMonad m where
  mixedReturn :: a -> m ct x y y' a
  return :: a -> m ct x x y' a
  (>>)   :: m Composable x y z a -> m ct y z z' b -> m ct x z z' b
  (>>=)  :: m Composable x y z a -> (a -> m ct y z z' b) -> m ct x z z' b
  fail   :: String -> m Composable x x y' ()

class IxMonadTrans t where
  lift :: IxMonad m => m ct x y z a -> t m ct x y z a

