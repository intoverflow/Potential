{-# LANGUAGE
	TypeFamilies,
	MultiParamTypeClasses,
	FunctionalDependencies #-}
module Language.Potential.Arch.SetGet where

class Setter field new ms where
  type Set field new ms
  set' :: field -> new -> ms -> Set field new ms

class Getter field ms where
  type Get field ms
  get' :: field -> ms -> Get field ms

class IsArg field reg | field -> reg where
  arg   :: field -> reg
  isArg :: field -> ()


