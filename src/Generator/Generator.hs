module Generator.Generator where

import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

generate :: (SymTable,AST) -> Result [(FilePath,String)]
generate = const $ Fail []
