{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Potential.DataStructure.PartialRelation where

class IsPartialOf struct partial | partial -> struct

