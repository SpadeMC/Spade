module Optimiser.Optimiser where

import           Language.AST (AST)

optimise :: AST -> AST
optimise = id
