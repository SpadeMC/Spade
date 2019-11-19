{-|
Module      : Results.Results
Description : Results for spade compiler stages
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines data-structures to be used for results in spade
-}
module Results.Results (Result (..)) where

import           Results.LogMsg (LogMsg (..))

data Result a = Pass a [LogMsg]
              | Fail [LogMsg]

instance Functor Result where
    fmap f (Pass x ws) = Pass (f x) ws
    fmap _ (Fail es)   = Fail es

instance Applicative Result where
    pure x = Pass x []
    (Pass f ws) <*> (Pass x ws') = Pass (f x) (ws ++ ws')
    (Pass _ ls) <*> (Fail ls') = Fail (ls ++ ls')
    (Fail ls) <*> (Pass _ ls') = Fail (ls ++ ls')
    (Fail ls) <*> (Fail ls') = Fail (ls ++ ls')

instance Monad Result where
    return = pure
    (Fail ls) >>= _ = Fail ls
    (Pass x ls) >>= f = case (f x) of
        Pass x' ls' -> Pass x' (ls ++ ls')
        Fail ls'    -> Fail (ls ++ ls')
