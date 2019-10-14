{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : LexerGen
Description : Lexer spec generator
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module generated Alex lexer specs from JSON and Haskell inputs.
-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List            (intercalate)
import           Data.Map             hiding (foldl)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.Text            (Text, append, pack)
import           GHC.Generics
import           Prelude              hiding (shows)

type LexerData = Map String TokenData

data TokenData =
    TokenData {
        lexeme          :: !String,
        literal         :: Maybe String,
        tokenValue      :: Maybe String,
        comment         :: Maybe String,
        extraFields     :: Maybe String,
        showExtraFields :: Maybe String,
        showBody        :: Maybe String
    } deriving (Show, Generic)

instance FromJSON TokenData
instance ToJSON TokenData

startFile :: FilePath
startFile = "SpadeLexer.x.start"

endFile :: FilePath
endFile = "SpadeLexer.x.end"

jsonFile :: FilePath
jsonFile = "SpadeLexer.x.json"

main :: IO ()
main = do
    d <- (eitherDecode <$> (B.readFile jsonFile)) :: IO (Either String LexerData)
    case d of
        Left err -> putStrLn err
        Right ps -> do
            start <- readFile startFile
            end <- readFile endFile
            putStrLn $ lexerToText start end (makeLexer ps)


lexerToText :: String -> String -> GeneratedLexer -> String
lexerToText start end lexer =
    start ++ (lexerToText' lexer) ++ end ++ "\n}"
        where
            lexerToText' :: GeneratedLexer -> String
            lexerToText' lexer = '\n' : unlines [ unlines (captureLines lexer)
                    , "{"
                    , "data LexemeClass = " ++ intercalate "\n\t\t| " (classes lexer)
                    , "\t deriving (Eq, Show)"
                    , ""
                    , "mkL :: LexemeClass -> AlexInput -> Int -> Alex Token"
                    , "mkL c (p, _, _, str) len = let t = take len str in case c of"
                    , '\t' : intercalate "\n\t" (classToTokens lexer)
                    , ""
                    , "alexEOF :: Alex Token"
                    , "alexEOF = return TEoF"
                    , ""
                    , "-- | Wrapper function for the lexer---allows the monadic lexer to be used with"
                    , "-- a monadic parser"
                    , "lexWrap :: (Token -> Alex a) -> Alex a"
                    , "lexWrap = (alexMonadScan >>=)"
                    , ""
                    , "-- | Type to represent tokens in the output stream"
                    , "data Token = " ++ intercalate "\n\t\t| " (tokens lexer)
                    , "\t| TEoF -- ^ @\\0@"
                    , "\t deriving (Eq, Ord)"
                    , ""
                    , "instance Show Token where"
                    , '\t' : intercalate "\n\t" (shows lexer)
                    , "\tshow TEoF = \"EoF\""
                    , ""
                ]

makeLexer :: LexerData -> GeneratedLexer
makeLexer d = foldl (<>) mempty $ makeLexer' <$> toList d
    where
        makeLexer' :: (String, TokenData) -> GeneratedLexer
        makeLexer' (name, d) = GeneratedLexer {
            captureLines = [ (quotify d) ++ " { mkL L" ++ name ++ " }" ]
            , classes = [ 'L' : name ]
            , classToTokens = [ 'L' : name ++ " -> return (T" ++ name ++ ' ' : (fromMaybeEmpty (tokenValue d)) ++ " p)" ]
            , tokens = [ 'T' : name ++ " { " ++ fromMaybeEmpty ((++ ",") <$> (extraFields d)) ++ "position :: AlexPosn } -- ^ " ++ fromMaybe ('@' : (toLiteral d) ++ "@") (comment d) ]
            , shows = [ "show (T" ++ name ++ ' ' : (fromMaybeEmpty $ showExtraFields d) ++ " _) = " ++ fromMaybe ('"' : (toLiteral d) ++ "\"") (showBody d) ]
        }
            where
                toLiteral d = fromMaybe (lexeme d) (literal d)
                quotify d = if head (lexeme d) == '@' then lexeme d else '"' : lexeme d ++ "\""
                fromMaybeEmpty = fromMaybe ""


data GeneratedLexer = GeneratedLexer {
    captureLines  :: [String],
    classes       :: [String],
    classToTokens :: [String],
    tokens        :: [String],
    shows         :: [String]
}

instance Monoid GeneratedLexer where
    mempty = GeneratedLexer [] [] [] [] []
    mappend (GeneratedLexer a b c d e) (GeneratedLexer a' b' c' d' e') = GeneratedLexer (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')
