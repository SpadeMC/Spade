{-|
Modu§e      : Main
Description : Entry-point for dig, compiler for Spade
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module provides a CLI for dig.
-}

module Main (main) where

import           ConstantProcessor.ConstantProcessor (processConstants)
import           Generator.Generator                 (generate)
import           Language.SpadeParserWrapper         (parseString)
import           Optimiser.Optimiser                 (optimise)
import           Results.Results                     (Errors, Result (..))
import           ScopeResolver.ScopeResolver         (resolveScope)
import           System.Exit                         (exitFailure)
import           System.IO                           (hPrint, stderr)
import           TypeChecker.TypeChecker             (checkTypes)

main :: IO ()
main = do
    -- Fetch the input
    c <- if True then
            getContents
        else
            readFile "asdf.sp"

    -- Output
    case compile c of
        Pass fs -> --if True then
                print fs
        --     else
        --         writeFile "asdf.mcfunction" $ show fs
        Fail es -> do
            printErrors es
            exitFailure

compile :: String -> Result [(FilePath,String)]
compile s = parseString s >>= resolveScope >>= checkTypes >>= processConstants >>= optimise >>= generate

printErrors :: Errors -> IO ()
printErrors [] = return ()
printErrors (e:es) = do
    hPrint stderr e
    printErrors es
