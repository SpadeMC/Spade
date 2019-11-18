module Optimiser.Optimiser where

import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

optimise :: (SymTable,AST) -> Result (SymTable,AST)
optimise = Pass . id
