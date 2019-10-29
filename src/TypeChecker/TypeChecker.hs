module TypeChecker.TypeChecker where

import           Language.AST (AST)

checkTypes :: AST -> Result AST
checkTypes = Fail []
