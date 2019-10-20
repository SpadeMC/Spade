{-|
Module      : Main
Description : Entry-point for dig, compiler for Spade
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module provides a CLI for dig.
-}

import           Modules.Results (Result (..))

main :: IO ()
main = putStrLn "Hello, world!"

compile :: String -> IO (Result [(String,String)])
compile = do
    a <- parse "-"
    checkTypes a
    optimise
    generate
