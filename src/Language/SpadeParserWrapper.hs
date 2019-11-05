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
    , parseFile
    , parseString
    ) where

import           Language.AST         (AST)
import           Language.SpadeLexer  (AlexPosn (AlexPn), runAlex)
import           Language.SpadeParser (parseSpade)
import           Results.Results      (Error (..), Result (..))

-- | Parse a file of "-" for @stdin@
parse :: String -> IO (Result AST)
parse "-" = parse' getContents
parse f   = parseFile f

-- | Parse an arbitrary file on disk
parseFile :: FilePath -> IO (Result AST)
parseFile name = parse' $ readFile name

-- | Parse an IO String (e.g. raw result of reading a file)
parse' :: IO String -> IO (Result AST)
parse' c = do
    s <- c
    return $ parseString s

-- | Parse a given string
parseString :: String -> Result AST
parseString s = case runAlex s parseSpade of
    Right x -> Pass x
    Left m  -> Fail [Error { message = m, locationInfo = loc }]
        where loc = AlexPn 0 (read . takeWhile (/= ':') $ m) (read . dropWhile (/= ':') . tail . takeWhile (/= ':') $ m)
