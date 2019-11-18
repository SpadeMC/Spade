module TypeChecker.TypeChecker (checkTypes) where

import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

checkTypes :: (SymTable,AST) -> Result (SymTable,AST)
checkTypes = const $ Fail []
