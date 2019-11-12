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

import           Language.AST         (AST)
import           Language.SpadeLexer  (AlexPosn (AlexPn), runAlex)
import           Language.SpadeParser (parseSpade)
import           Results.Results      (Error (..), Result (..))

-- | Parse a file of "-" for @stdin@
parse :: FilePath -> IO (Result AST)
parse "-" = parseString <$> getContents
parse f   = parseString <$> readFile f

-- | Parse a given string
parseString :: String -> Result AST
parseString s = case runAlex s parseSpade of
    Right x -> Pass x
    Left m  -> Fail [Error { message = m, locationInfo = loc }]
        -- TODO: Get real location
        -- where loc = AlexPn 0 (read . takeWhile (/= ':') $ m) (read . dropWhile (/= ':') . tail . takeWhile (/= ':') $ m)
        where loc = AlexPn 0 0 0
