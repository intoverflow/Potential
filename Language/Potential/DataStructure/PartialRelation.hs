{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Potential.DataStructure.PartialRelation where

class IsPartialOf struct partial | partial -> struct

