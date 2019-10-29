module Generator.Generator where

import           Language.AST    (AST)
import           Results.Results (Results)

generate :: AST -> Result [(String,String)]
generate = Fail []
