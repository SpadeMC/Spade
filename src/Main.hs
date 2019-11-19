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
    -- Fetch the input
    c <- if True then
            getContents
        else
            readFile "asdf.sp"

    -- Output
    case compile c of
        Pass fs ls -> do
            printLogMsgs ls
            print fs
                -- writeFile "asdf.mcfunction" $ show fs
        Fail es -> do
            printLogMsgs es
            exitFailure

compile :: String -> Result [(FilePath,String)]
compile s = parseString s >>= resolveScope >>= checkTypes >>= processConstants >>= optimise >>= generate



-- printCompilerMsgs :: [CompilerMsg] -> IO ()
-- printCompilerMsgs [] = return ()
-- printCompilerMsgs (e:es) = do
--     hPrint stderr e
--     printErrors es

-- printCompilerMsgs :: Result a -> IO ()
-- printCompilerMsgs (Pass _ ws)  = sequence_ $ printWarning <$> ws
-- printCompilerMsgs (Fail es ws) = sequence_ $ printWarning <$> ws >>= sequence_ $ printWarning <$> es

-- printWarning :: Warning -> IO ()
-- printWarning w = print $ "Warning: " ++ w

-- printError :: Error -> IO ()
-- printError e = print $ "Error: " ++ e
