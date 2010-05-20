{-# LANGUAGE
	NoImplicitPrelude #-}
module Potential.PMonadc
	( get, set, pTell
	, mixedReturn, pReturn, (>>>), (>>>=), PState(..)
	) where

import Potential.PMonad

pReturn :: (PMonad m) => a -> m c x x a
pReturn = return

(>>>) :: (PMonad m) => m c x y a -> m c y z b -> m c x z b
(>>>) = (>>)

(>>>=) :: (PMonad m) => m c x y a -> (a -> m c y z b) -> m c x z b
(>>>=) = (>>=)

