{-# LANGUAGE
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances,
	IncoherentInstances,
	FunctionalDependencies,
	EmptyDataDecls #-}
module Potential.Handles
	( Allocator, HS, HZ, C, N
	, handleIsOpen', free', alloc', realloc'
	, HandleIsOpen, MaybeHandleIsOpen
	, Free, MaybeFree
	, True, False
	) where

import Potential.Constraints

-- The allocator
data Allocator hn hs cs = Allocator hn hs cs

alloc' :: MaybeAlloc (Allocator hn hs cs) hn (Allocator hn' hs' cs) c
       => c -> Allocator hn hs cs -> (Allocator hn' hs' cs, hn)
alloc' _ = undefined

handleIsOpen' :: MaybeHandleIsOpen alloc h c
	      => c -> alloc -> h -> ()
handleIsOpen' _ _ _ = ()

free' :: MaybeFree (Allocator hn hs cs) h (Allocator hn hs' cs') c =>
	c -> (Allocator hn hs cs) -> h -> Allocator hn hs' cs'
free' _ _ _ = undefined

realloc' :: MaybeRealloc (Allocator hn hs cs) h (Allocator hn hs' cs') c
	 => c -> Allocator hn hs cs -> h -> Allocator hn hs' cs'
realloc' _ _ = undefined


class MaybeAlloc alloc h alloc' c | alloc c -> h alloc'
instance Alloc alloc h alloc' => MaybeAlloc alloc h alloc' ConstraintsOn
instance MaybeAlloc alloc h alloc' ConstraintsOff

class MaybeHandleIsOpen alloc h c
instance (HandleIsOpen alloc h) => MaybeHandleIsOpen alloc h ConstraintsOn
instance MaybeHandleIsOpen alloc h ConstraintsOff

class MaybeFree alloc h alloc' c | alloc h c -> alloc'
instance Free alloc h alloc' => MaybeFree alloc h alloc' ConstraintsOn
instance MaybeFree alloc h alloc' ConstraintsOff

class MaybeRealloc alloc h alloc' c | alloc h c -> alloc'
instance Realloc alloc h alloc' => MaybeRealloc alloc h alloc' ConstraintsOn
instance MaybeRealloc alloc h alloc' ConstraintsOff


class Alloc alloc h alloc' | alloc -> h alloc'
instance ( AddToList hn hs hs', InList hn hs False, InList (HS hn) hs' False )
  => Alloc (Allocator hn hs cs) hn (Allocator (HS hn) hs' cs)

class HandleIsOpen alloc h
instance (InList h hs True, InList h cs False)
  => HandleIsOpen (Allocator hn hs cs) h

class Free alloc h alloc' | alloc h -> alloc'
instance (RemoveFromList h hs hs', AddToList h cs cs')
  => Free (Allocator hn hs cs) h (Allocator hn hs' cs')

class Realloc alloc h alloc' | alloc h -> alloc'
instance (RemoveFromList h cs cs', AddToList h hs hs')
  => Realloc (Allocator hn hs cs) h (Allocator hn hs' cs')


-- Things used to manage the list of open handles

data HZ = HZ
data (List a) => HS a = HS a

class Handle a
instance Handle HZ
instance Handle r => Handle (HS r)

data N = N
data (InList a b False, Handle a, List b) => C a b = C (a, b)

class List a
instance List N
instance List r => List (C a r)

data True
data False

class Logical a
instance Logical True
instance Logical False

class EQN n1 n2 t | n1 n2 -> t
instance EQN HZ HZ True
instance (EQN HZ a False) => EQN HZ (HS a) False
instance (EQN a HZ False) => EQN (HS a) HZ False
instance (EQN n1 n2 t) => EQN (HS n1) (HS n2) t
instance EQN n1 n1 True
instance (EQN a b t) => EQN b a t

class LOr n1 n2 t | n1 n2 -> t
instance LOr True n2 True
instance LOr n1 True True
instance LOr False False False
instance LOr a b t => LOr b a t

class IsFalse a
instance (LOr a b False) => IsFalse a

class LAnd n1 n2 t | n1 n2 -> t
instance LAnd True True True
instance LAnd False a False
instance LAnd a False False

class IsTrue a
instance (LAnd a b True) => IsTrue a


class InList a as t | a as -> t
instance InList a N False
instance (EQN a a' t, InList a r t', LOr t t' t'') => InList a (C a' r) t''

class ( InList a as False, InList a as' True )
 => AddToList a as as' | a as -> as'
instance (InList a as False, InList a (C a as) True) => AddToList a as (C a as)


class Decide i th el choice | i th el -> choice
instance Decide True  th el th
instance Decide False th el el


class (InList a as True, InList a as' False)
  => RemoveFromList a as as' | a as -> as'
instance RemoveFromList a (C a N) N
instance ( EQN a b t, Decide t as' (C b as') choice, RemoveFromList a as as'
	 , InList a (C b as) True, InList a choice False )
  => RemoveFromList a (C b as) choice

