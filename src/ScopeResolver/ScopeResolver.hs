module ScopeResolver.ScopeResolver (resolveScope, SymTable) where

import           Args            (Args)
import           Data.Map.Lazy   (Map, empty)
import           Language.AST    (AST, Ident)
import           Results.Results (Result (..))

type SymTable = Map Ident Int

resolveScope :: Args -> AST -> Result (SymTable,AST)
resolveScope _ a = pure (empty, a)
