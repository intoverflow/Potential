{-# LANGUAGE
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances,
	TypeFamilies,
	EmptyDataDecls #-}
module Potential.Handles
	( Allocator, HS, HZ, C, N
	, handleIsOpen', free', alloc', realloc'
	, HandleIsOpen, MaybeHandleIsOpen
	, Free, MaybeFree, MaybeFreedAlloc
	, True, False, IsEq
	) where

import Potential.Constraints

-- The allocator
data Allocator hnext hs = Allocator hnext hs
getHS :: Allocator hnext hs -> hs
getHS _ = undefined
setHS :: Allocator hnext hs -> hs' -> Allocator hnext hs'
setHS _ _ = undefined

alloc' :: Allocator hnext hs -> (Allocator (HS hnext) (Added hnext hs), hnext)
alloc' _ = undefined

handleIsOpen' :: (MaybeHandleIsOpen alloc h c) => c -> alloc -> h -> ()
handleIsOpen' _ _ _ = ()

free' :: MaybeFree (Allocator hn hs) h c =>
	c -> (Allocator hn hs) -> h -> MaybeFreedAlloc (Allocator hn hs) h c
free' _ _ _ = undefined

realloc' :: Allocator hnext hs -> h -> Allocator hnext (Added h hs)
realloc' _ _ = undefined

class HandleIsOpen alloc h
instance (IsInList h hs ~ True) => HandleIsOpen (Allocator hnext hs) h

class MaybeHandleIsOpen alloc h c
instance (HandleIsOpen alloc h) => MaybeHandleIsOpen alloc h ConstraintsOn
instance MaybeHandleIsOpen alloc h ConstraintsOff

class Free alloc h where type FreedAlloc alloc h
instance (RemoveFromList h hs) => Free (Allocator hnext hs) h where
  type FreedAlloc (Allocator hnext hs) h = Allocator hnext (Removed h hs)

class MaybeFree alloc h c where type MaybeFreedAlloc alloc h c
instance Free (Allocator hnext hs) h =>
  MaybeFree (Allocator hnext hs) h ConstraintsOn where
    type MaybeFreedAlloc (Allocator hnext hs) h ConstraintsOn
	= FreedAlloc (Allocator hnext hs) h
instance MaybeFree (Allocator hnext hs) h ConstraintsOff where
    type MaybeFreedAlloc (Allocator hnext hs) h ConstraintsOff
	= Allocator hnext hs



-- Things used to manage the list of open handles

data HZ = HZ
data (List a) => HS a = HS a

class Handle a
instance Handle HZ
instance Handle r => Handle (HS r)

data N = N
data (IsInList a b ~ False, Handle a, List b) => C a b = C (a, b)

class List a
instance List N
instance List r => List (C a r)

data True
data False

class Logical a
instance Logical True
instance Logical False

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
  type LAnd n1 n2
  type LOr  n1 n2
instance Logic True True where
  type LAnd True True = True
  type LOr  True True = True
instance Logic True False where
  type LAnd True False = False
  type LOr  True False = True
instance Logic False True where
  type LAnd False True = False
  type LOr  False True = True
instance Logic False False where
  type LAnd False False = False
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

