module ScopeResolver.ScopeResolver (resolveScope, SymTable) where

import           Data.Map.Lazy   (Map)
import           Language.AST    (AST, Ident)
import           Results.Results (Result (..))

type SymTable = Map Ident Int

resolveScope :: AST -> Result (SymTable,AST)
resolveScope = const $ Fail []
