module Optimiser.Optimiser (optimise) where

import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

optimise :: (SymTable,AST) -> Result (SymTable,AST)
optimise = Pass
