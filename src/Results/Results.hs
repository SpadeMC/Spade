{-|
Module      : Results.Results
Description : Results for dig stages
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines data-structures to be used for results in dig
-}
module Results.Results (Result (..), Error(..), Errors) where

import           Results.Errors (Error (..), Errors)

data Result a = Pass a
              | Fail Errors

instance Functor Result where
    fmap f (Pass x)  = Pass $ f x
    fmap _ (Fail es) = Fail es

instance Applicative Result where
    pure = Pass
    (Pass f) <*> (Pass x) = Pass $ f x
    _ <*> (Fail es) = Fail es
    (Fail es) <*> _ = Fail es

instance Monad Result where
    return = pure
    (Fail es) >>= _ = Fail es
    (Pass x) >>= f = f x
