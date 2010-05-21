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

alloc' :: ( MaybeAddToList hn hs hs' c
	  , MaybeInList hn hs False c
	  , MaybeInList (HS hn) hs' False c )
       => c -> Allocator hn hs cs -> (Allocator (HS hn) hs' cs, hnext)
alloc' _ = undefined

handleIsOpen' :: (MaybeHandleIsOpen alloc h c) => c -> alloc -> h -> ()
handleIsOpen' _ _ _ = ()

free' :: MaybeFree (Allocator hn hs cs) h (Allocator hn hs' cs') c =>
	c -> (Allocator hn hs cs) -> h -> Allocator hn hs' cs'
free' _ _ _ = undefined

realloc' :: ( MaybeAddToList h hs hs' c
	    , MaybeInList h cs True c
	    , MaybeRemove h cs cs' c)
	 => c -> Allocator hnext hs cs -> h -> Allocator hnext hs' cs'
realloc' _ _ = undefined

class MaybeAddToList hn hs hs' c | hn hs c -> hs'
instance (AddToList hn hs hs') => MaybeAddToList hn hs hs' ConstraintsOn
instance MaybeAddToList hn hs hs ConstraintsOff

class MaybeInList hn hs t c
instance (InList hn hs t) => MaybeInList hn hs t ConstraintsOn
instance MaybeInList hn hs t ConstraintsOff

class MaybeRemove h cs cs' c
instance (RemoveFromList h cs cs') => MaybeRemove h cs cs' ConstraintsOn
instance MaybeRemove h cs cs ConstraintsOff

class HandleIsOpen alloc h
instance (InList h hs True, InList h cs False) => HandleIsOpen (Allocator hn hs cs) h

class MaybeHandleIsOpen alloc h c
instance (HandleIsOpen alloc h) => MaybeHandleIsOpen alloc h ConstraintsOn
instance MaybeHandleIsOpen alloc h ConstraintsOff

class Free alloc h alloc' | alloc h -> alloc'
instance (InList h hs' False, RemoveFromList h hs hs', AddToList h cs cs')
  => Free (Allocator hn hs cs) h (Allocator hn hs' cs')

class MaybeFree alloc h alloc' c | alloc h c -> alloc'
instance Free alloc h alloc' => MaybeFree alloc h alloc' ConstraintsOn
instance MaybeFree alloc h alloc' ConstraintsOff


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

<<<<<<< HEAD
class EQN n1 n2 t | n1 n2 -> t
instance EQN HZ HZ True
instance (EQN HZ a False) => EQN HZ (HS a) False
instance (EQN a HZ False) => EQN (HS a) HZ False
instance (EQN n1 n2 t) => EQN (HS n1) (HS n2) t
instance EQN n1 n1 True

class LOr n1 n2 t | n1 n2 -> t
instance LOr True n2 True
instance LOr n1 True True
instance LOr False False False

class LAnd n1 n2 t | n1 n2 -> t
instance LAnd True True True
instance LAnd False a False
instance LAnd a False False

class InList a as t | a as -> t
instance InList a N False
instance (EQN a a' t, InList a r t', LOr t t' t'') => InList a (C a' r) t''

class Decide a b c t | a b t -> c
instance Decide a b a True
instance Decide a b b False

class (InList a as False, InList a as' True)
  => AddToList a as as' | a as -> as'
instance (InList a as False) => AddToList a as (C a as)

class (InList a as True, InList a as' False) => RemoveFromList a as as'
instance ( InList a (C a' r) True
	 , EQN a a' eq
	 , Decide r (C a' r) c eq
	 , InList a c False)
  => RemoveFromList a (C a' r) c

=======
--class ( IsEq n1 n1 ~ True
--      , IsEq n2 n2 ~ True
--      ) => EQN n1 n2 where type IsEq n1 n2
class EQN n1 n2 where type IsEq n1 n2
instance EQN HZ HZ where type IsEq HZ HZ = True
instance Handle a => EQN HZ (HS a) where type IsEq HZ (HS a) = False
instance Handle a => EQN (HS a) HZ where type IsEq (HS a) HZ = False
instance EQN n1 n2 =>
  EQN (HS n1) (HS n2) where type IsEq (HS n1) (HS n2) = IsEq n1 n2

class Logic n1 n2 where
  type LOr  n1 n2
instance Logic True a where
  type LOr  True a = True
instance Logic a False where
  type LOr  a False = True
instance Logic False False where
  type LOr  False False = False

class InList a as where
  type IsInList a as
instance Handle a => InList a N where
  type IsInList a N = False
instance (EQN a a', InList a r) => InList a (C a' r) where
  type IsInList a (C a' r) = LOr (IsEq a a') (IsInList a r)

class Decide a b c where type Choose a b c
instance Decide a b True  where type Choose a b True  = a
instance Decide a b False where type Choose a b False = b

--class (IsInList a as ~ False, IsInList a (Added a as) ~ True)
class AddToList a as where
      type Added a as
instance (IsInList a as ~ False) => AddToList a as where
  type Added a as = C a as

--class (IsInList a (Removed a as) ~ False) => RemoveFromList a as where
class RemoveFromList a as where
  type Removed a as
instance ( IsInList a (C a' r) ~ True, EQN a a')
  => RemoveFromList a (C a' r) where
      type Removed a (C a' r) = Choose r (C a' r) (IsEq a a')

assertIsInList :: (IsInList a as ~ True) => a -> as -> ()
assertIsInList _ _ = ()

assertIsNotInList :: (IsInList a as ~ False) => a -> as -> ()
assertIsNotInList _ _ = ()

removeFromList :: (IsInList a as ~ True) => a -> as -> Removed a as
removeFromList _ _ = undefined

asList :: C a r -> C a r
asList x = x

h0 = undefined :: HZ
h1 = undefined :: HS HZ
h2 = undefined :: HS (HS HZ)
h3 = undefined :: HS (HS (HS HZ))
h4 = undefined :: HS (HS (HS (HS HZ)))
testList = C (h1, C (h0, C (h4, N)))
--testList'  = C (h0, C (h0, C (h4, N)))
--testList'' = C (h4, C (h0, C (h4, N)))
>>>>>>> initial-blog-announce

