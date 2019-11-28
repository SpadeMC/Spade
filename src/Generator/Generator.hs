module Generator.Generator (generate) where

import           Args            (Args)
import           Language.AST                (AST)
import           Results.Results             (Result (..))
import           ScopeResolver.ScopeResolver (SymTable)

generate :: Args -> (SymTable,AST) -> Result [(FilePath,String)]
generate _ = const $ Pass [] []
