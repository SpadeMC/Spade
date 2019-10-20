{-|
Module      : Errors
Description : Error-handling for dig
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines data structures to nicely handle errors encountered during compilation
-}

module Results.Errors (Error(..), Errors) where

import           Language.SpadeLexer (AlexPosn (..))

type Errors = [Error]

data Error = Error { locationInfo :: AlexPosn, message :: String }

instance Show Error where
    show (Error (AlexPn l n _) m) = show l ++ ':' : show n ++ ' ' : show m
