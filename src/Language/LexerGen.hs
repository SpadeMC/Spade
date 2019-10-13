{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Prelude hiding (unlines, shows, head, foldl)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Text
import Data.Map hiding (foldl)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Monoid

type LexerData = Map Text TokenData

data TokenData =
    TokenData {
        lexeme          :: !String,
        literal         :: Maybe Text,
        tokenValue      :: Maybe Text,
        comment         :: Maybe Text,
        extraFields     :: Maybe Text,
        showExtraFields :: Maybe Text,
        showBody        :: Maybe Text
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
            start <- B.readFile startFile
            end <- B.readFile endFile
            putStrLn $ lexerToText start end (makeLexer d)


lexerToText :: Text -> Text -> GeneratedLexer -> Text
lexerToText start end lexer =
    start ++ (lexerToText' lexer) ++ end
        where
            lexerToText' :: GeneratedLexer -> Text
            lexerToText' lexer = '\n' : unlines [ unlines (captureLines lexer)
                    , "{"
                    , "data LexemeClass = " ++ intercalate "\n\t\t| " (classes lexer)
                    , "\t deriving (Eq, Show)"
                    , "mkL :: LexemeClass -> AlexInput -> Int -> Alex Token"
                    , "mkL c (p, _, _, str) len = let t = take len str in case c of"
                    , '\t' : intercalate "\n\t" (classToTokens lexer)
                    , "alexEOF :: Alex Token"
                    , "alexEOF = return TEoF"
                    , ""
                    , "-- | Wrapper function for the lexer---allows the monadic lexer to be used with"
                    , "-- a monadic parser"
                    , "lexWrap :: (Token -> Alex a) -> Alex a"
                    , "lexWrap = (alexMonadScan >>=)"
                    , "-- | Type to represent tokens in the output stream"
                    , "data Token = " ++ intercalate "\n\t\t| " (classes tokens)
                    , "\t deriving (Eq, Ord)"
                    , "instance Show Token where"
                    , '\t' :  intercalate "\n\t" (shows lexer)
                    , ""
                ]

makeLexer :: LexerData -> GeneratedLexer
makeLexer d = foldl (<>) $ makeLexer' <$> fromList d
    where
        makeLexer' :: (Text, TokenData)
        makeLexer' (name, d) = GeneratedLexer {
            captureLines = (quotify d) ++ " { mkL L" ++ name ++ " }"
            , classes = 'L' : name
            , classToTokens = 'L' : name ++ " -> return (T" ++ name ++ ' ' : (fromMaybeEmpty (tokenValue d)) ++ " p)"
            , tokens = 'T' : name ++ "{ " ++ fromMaybeEmpty ((++ ",") <$> (extraFields d)) ++ "position :: AlexPosn } -- ^ " fromMaybe ('@' : (toLiteral d) ++ "@") (comment d)
            , shows = "show (T" ++ name ++ ' ' : (fromMaybeEmpty showExtraFields) ++ " _) = " ++ fromMaybe (toLiteral d) (showBody d)
        }
            where
                toLiteral d = fromMaybe (lexeme d) (literal d)
                quotify d = if head (lexeme d) == '@' then lexeme d else '"' : lexeme d ++ "\""
                fromMaybeEmpty = fromMaybe ""


data GeneratedLexer = GeneratedLexer {
    captureLines :: [Text],
    classes :: [Text],
    classToTokens :: [Text],
    tokens :: [Text],
    shows :: [Text]
}

instance Monoid GeneratedLexer where
    mempty = GeneratedLexer [] [] [] [] []
    mappend (GeneratedLexer a b c d e) (GeneratedLexer a' b' c' d' e') = GeneratedLexer (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')
