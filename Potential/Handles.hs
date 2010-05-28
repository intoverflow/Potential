{-# LANGUAGE
	TypeFamilies,
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances,
	IncoherentInstances,
	EmptyDataDecls #-}
module Potential.Handles
	( Allocator, HS, HZ, C, N
	, handleIsOpen', free', alloc', realloc'
	, HandleIsOpen, MaybeHandleIsOpen
	, MaybeFree, MaybeAlloc
	, True, False
	) where

import Potential.Constraints

-- The allocator
data Allocator hn hs = Allocator hn hs

-- alloc' :: (AddToList hn hs hs') => c -> Allocator hn hs -> (Allocator (HS hn) hs', hn)
alloc' :: (MaybeAlloc (Allocator hn hs) hn (Allocator hn' hs') c)
	=> c -> Allocator hn hs -> (Allocator hn' hs', hn)
alloc' _ = undefined

handleIsOpen' :: MaybeHandleIsOpen alloc h c
	      => c -> alloc -> h -> ()
handleIsOpen' _ _ _ = ()

free' :: MaybeFree (Allocator hn hs)
		   h
		   (Allocator hn hs') c =>
	c -> (Allocator hn hs) -> h -> Allocator hn hs'
free' _ _ _ = undefined

--realloc' :: (AddToList h hs hs') => c -> Allocator hn hs -> h -> Allocator hn hs'
realloc' :: (MaybeRealloc (Allocator hn hs) h (Allocator hn hs') c)
	  => c -> Allocator hn hs -> h -> Allocator hn hs'
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
instance ( InList hn hs False
	 , InList (HS hn) hs' False
	 , AddToList hn hs hs'
	 ) => Alloc (Allocator hn hs) hn (Allocator (HS hn) hs')

class HandleIsOpen alloc h
instance ( InList h hs True
	 ) => HandleIsOpen (Allocator hn hs) h

class Free alloc h alloc' | alloc h -> alloc'
instance ( InList h hs True
	 , RemoveFromList h hs hs'
	 , InList h hs' False
	 )
  => Free (Allocator hn hs) h (Allocator hn hs')

class Realloc alloc h alloc' | alloc h -> alloc'
instance (AddToList h hs hs') => Realloc (Allocator hn hs) h (Allocator hn hs')


-- Things used to manage the list of open handles

data HZ
data HS a

class Handle a
instance Handle HZ
instance Handle r => Handle (HS r)


data N
data C a inout as

class List a
instance List N
instance (Handle a, List r) => List (C a t r)


data True
data False

class Logical a
instance Logical True
instance Logical False


class EQN n1 n2 t | n1 n2 -> t
-- Base case for handles
instance EQN HZ HZ True
-- Inductive instructions for handles
instance (EQN HZ a False) => EQN HZ (HS a) False
instance (EQN a HZ False) => EQN (HS a) HZ False
instance (EQN n1 n2 t) => EQN (HS n1) (HS n2) t
instance EQN n n True

class LOr n1 n2 t | n1 n2 -> t
instance LOr True n2 True
instance LOr n1 True True
instance LOr False False False

instance LOr a b t => LOr b a t
instance LOr a False True => LOr True True a
instance LOr a False False => LOr False False a

class LAnd n1 n2 t | n1 n2 -> t
instance LAnd True True True
instance LAnd False a False
instance LAnd a False False

instance LAnd a b t => LAnd b a t
instance LAnd a True False => LAnd False False a


class LNot a b | a -> b
instance LNot True False
instance LNot a b => LNot b a


class (Handle a, List as) => InList a as t | a as -> t
instance Handle a => InList a N False
instance ( Handle a
	 , Handle a'
	 , EQN a a' isHead
	 , InList a as inRest
	 , Decide isHead headT inRest inList
	 ) => InList a (C a' headT as) inList


class Decide i th el choice | i th el -> choice
instance Decide True  th el th
instance Decide False th el el


class ( Handle a
      , List as
      , List as'
      , InList a as True
      , InList a as' False
      ) => RemoveFromList a as as' | a as -> as'
instance ( InList a as True
	 , InList a (C a False as) False
	 ) => RemoveFromList a as (C a False as)

class ( Handle a
      , List as
      , List as'
      , InList a as False
      , InList a as' True
      ) => AddToList a as as' | a as -> as'
instance ( Handle a
	 , List as
	 , InList a as False
	 , InList a (C a True as) True
	 ) => AddToList a as (C a True as)

