module TypeChecker.TypeChecker (checkTypes) where

import           Language.AST    (AST)
import           Results.Results (Result (..))


checkTypes :: AST -> Result AST
checkTypes = const $ Fail []
