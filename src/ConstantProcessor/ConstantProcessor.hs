module ConstantProcessor.ConstantProcessor (processConstants) where

import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

processConstants :: (SymTable,AST) -> Result (SymTable,AST)
processConstants = Pass . id
