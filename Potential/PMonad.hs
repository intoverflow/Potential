{-# LANGUAGE
	NoMonomorphismRestriction,
	UndecidableInstances,
	FlexibleContexts,
	NoImplicitPrelude #-}
module Potential.PMonad ( PMonad(..), PState(..)
			, get, set, pTell, getAlloc, setAlloc, getCmp, setCmp
			, handleIsOpen, alloc, free, realloc
			) where

import Prelude ( (++), String, undefined, ($) )

import Potential.Constraints
import Potential.MachineState
import Potential.Handles

class PMonad m where
  mixedReturn :: a -> m c x y a
  return :: a -> m c x x a
  (>>)   :: m c x y a -> m c y z b -> m c x z b
  (>>=)  :: m c x y a -> (a -> m c y z b) -> m c x z b
  fail   :: String -> m c x x ()
  fail = fail

data PState l c s1 s2 a = PState { runPState :: c -> s1 -> (a, s2, [l]) }

getConstraints :: PState l c x x c
getConstraints = return undefined

instance PMonad (PState l) where
   mixedReturn a = PState (\_ s -> (a, undefined, []))
   return a = PState (\_ s -> (a, s, []))
   f >>= m  = PState (\c s1 -> let (a, s2, l)   = runPState f c s1
				   (a', s3, l') = runPState (m a) c s2
			       in (a', s3, l ++ l'))
   m1 >> m2 = m1 >>= \_ -> m2

pGet   = PState (\_ s -> (s, s, []))
pPut s = PState (\_ _ -> ((), s, []))
pModify f = pGet >>= \x -> pPut (f x)
pTell l = PState (\_ s -> ((), s, l))

-- In order to avoid madness, we don't export pGet, pPut, or pModify,
-- which have too much of an effect on our Hoare types to be safe to release
get field =
     do ms <- pGet
        let fdata = get' field ms
        return fdata
set field new = pModify (\ms -> set' field new ms)

getAlloc =
     do ms <- pGet
	return $ getAlloc' ms
setAlloc a = pModify (setAlloc' a)

handleIsOpen h =
     do alloc <- getAlloc
	c <- getConstraints
	return $ handleIsOpen' c alloc h

alloc =
     do a <- getAlloc
	c <- getConstraints
	let (a', h) = alloc' c a
	setAlloc a'
	return h

free h =
     do alloc <- getAlloc
	c <- getConstraints
	setAlloc $ free' c alloc h
	return ()

realloc h =
     do alloc <- getAlloc
	c <- getConstraints
	setAlloc $ realloc' c alloc h
	return ()

getCmp =
     do ms <- pGet
	return $ getCmp' ms
setCmp a = pModify (setCmp' a)

