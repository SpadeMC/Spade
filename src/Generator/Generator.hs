module Generator.Generator where

import           Language.AST    (AST)
import           Results.Results (Result (..))

generate :: AST -> Result [(FilePath,String)]
generate = const $ Fail []
