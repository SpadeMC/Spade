module Optimiser.Optimiser (optimise) where

import           Args                        (Args)
import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

optimise :: Args -> (SymTable,AST) -> Result (SymTable,AST)
optimise _ = pure
