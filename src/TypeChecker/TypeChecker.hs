module TypeChecker.TypeChecker (checkTypes) where

import           Args                        (Args)
import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

checkTypes :: Args -> (SymTable,AST) -> Result (SymTable,AST)
checkTypes _ = pure
