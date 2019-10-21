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

import           Generator.Generator         (generate)
import           Language.SpadeParserWrapper (parse)
import           Modules.Results             (Result (..))
import           Optimiser.Optimiser         (optimise)
import           TypeChecker.TypeChecker     (typeCheck)

main :: IO ()
main = do
    -- Fetch the input
    c <- if True then
            getContents
        else
            readFile "asdf.sp"
    -- Compile the code
    let rs = compile c

    -- Output
    case rs of
        Pass fs -> if True then
                print rs
            else
                writeFile "asdf.mcfunction"
        Fail es -> printErrors es

compile :: String -> Result [(String,String)]
compile = do
    checkTypes a
    optimise
    generate

printErrors :: Errors -> IO ()
printErrors [] = return ()
printErrors (e:es) = do
    print e
    printErrors es
