{-|
Module      : SpadeParserWrapper
Description : Wrapper for the Spade parser and lexer
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This forms a wrapper to make the Spade parser and lexer easier to use.
It extends functionality by allowing the format of strings, arbitrary files on disk, and @stdin@ when the file "-" is used.
-}
module Language.SpadeParserWrapper
    ( AST
    , parse
    , parseString
    ) where

import           Args                 (Args)
import           Language.AST         (AST)
import           Language.SpadeLexer  (AlexPosn (AlexPn), runAlex)
import           Language.SpadeParser (parseSpade)
import           Results.LogMsg       (LogMsg (..), Severity (..))
import           Results.Results      (Result (..))

-- | Parse a file of "-" for @stdin@
parse :: Args -> FilePath -> IO (Result AST)
parse a "-" = parseString a <$> getContents
parse a f   = parseString a <$> readFile f

-- | Parse a given string
parseString :: Args -> String -> Result AST
parseString _ s = case runAlex s parseSpade of
    Right x -> pure x
    Left m  -> Fail [LogMsg { severity = Error, message = m, locationInfo = Just loc }]
        -- TODO: Get real location
        -- where loc = AlexPn 0 (read . takeWhile (/= ':') $ m) (read . dropWhile (/= ':') . tail . takeWhile (/= ':') $ m)
        where loc = AlexPn 0 0 0
