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
import           Language.SpadeParserWrapper (parseString)
-- import           Optimiser.Optimiser         (optimise)
import           Results.Results             (Errors, Result (..))
import           System.Exit                 (exitFailure)
import           System.IO                   (hPutStrLn, stderr)
import           TypeChecker.TypeChecker     (checkTypes)

main :: IO ()
main = do
    -- Fetch the input
    c <- if True then
            getContents
        else
            readFile "asdf.sp"

    -- Compile the code
    rs <- compile c

    -- Output
    case rs of
        Pass fs -> if True then
                print fs
            else
                writeFile "asdf.mcfunction" $ show fs
        Fail es -> do
            printErrors es
            exitFailure

compile :: String -> IO (Result [(FilePath,String)])
compile s = do
    -- parsed <- parse s
    let parsed = parseString s
    -- do
    --     checkTypes
    --     return . optimise
    --     return . generate
    -- parsed :: Result AST
    -- checkTypes :: AST -> Result AST
    let typed = parsed >>= checkTypes
    -- Add the preprocessor
    -- optimise
    return (typed >>= generate)

printErrors :: Errors -> IO ()
printErrors [] = return ()
printErrors (e:es) = do
    hPutStrLn stderr $ show e
    printErrors es
