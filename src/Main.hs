{-|
Modu§e      : Main
Description : Entry-point for spade
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module provides a CLI for spade.
-}

module Main (main) where

import           Args                                (Args, inputFile,
                                                      outputFile, parseArgv)
import           ConstantProcessor.ConstantProcessor (processConstants)
import           Generator.Generator                 (generate)
import           Language.SpadeParserWrapper         (parseString)
import           Optimiser.Optimiser                 (optimise)
import           Results.LogMsg                      (printLogMsgs)
import           Results.Results                     (Result (..))
import           ScopeResolver.ScopeResolver         (resolveScope)
import           System.Exit                         (exitFailure)
import           TypeChecker.TypeChecker             (checkTypes)

main :: IO ()
main = do
    -- Parse the commandline arguments
    a <- parseArgv
    print a

    -- Fetch the input
    c <- if inputFile a == "-" then
            getContents
        else
            readFile $ inputFile a

    -- Compile and output
    case compile a c of
        Pass fs ls -> do
            printLogMsgs ls
            if outputFile a == "-" then
                print fs
            else
                writeFile (outputFile a) $ show fs
        Fail es -> do
            printLogMsgs es
            exitFailure

compile :: Args -> String -> Result [(FilePath,String)]
compile a s = parseString a s
    >>= resolveScope a
    >>= checkTypes a
    >>= processConstants a
    >>= optimise a
    >>= generate a
