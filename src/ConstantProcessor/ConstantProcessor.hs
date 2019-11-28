module ConstantProcessor.ConstantProcessor (processConstants) where

import           Args                        (Args)
import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

processConstants :: Args -> (SymTable,AST) -> Result (SymTable,AST)
processConstants _ = pure
