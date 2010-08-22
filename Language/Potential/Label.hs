module Language.Potential.Label where

import Language.Potential.Core

label s =
     do instr $ Label s

