{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-|
Module      : SpadeLexer
Description : Lexer for the spade language
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines the machinery to lexically analyse the Spade language given in an input string.
-}
module Parser.EmperorLexer (Alex, AlexPosn(..), Token(..), lexWrap, alexError, runAlex) where

}

%wrapper "monad"

$alpha = [A-Za-z]
$num = [0-9]
$alphaNum = [$alpha$num]
@newline = \r\n | \r | \n

@tabs = \t+
@spaces = \ +
@whitespace = @tabs | @spaces

@ident = $alpha $alphaNum*
@int = $num+
@real = $num+ \. $num+
@bool = (true) | (false)
$stringchar = [^\n"]
@string = \'$stringchar*\'

@partSeparator = (";" @newline*) | (@newline+)
@blockStarter = "{" @newline*
@blockSeparator = "}" @newline*

@lineComment = # .* (@newline @whitespace?)+
@ignoredWhitespace = \\ @newline @whitespace*

:-

-- Things to ignore
@spaces             ;
@tabs+              ;
@lineComment        ;
@ignoredWhitespace  ;

-- Values
@int                { mkL LInteger }
@bool               { mkL LBool }
@real               { mkL LReal }
@string             { mkL LString }

-- Types
"int"               { mkL LIntT }
"bool"              { mkL LBoolT }
"real"              { mkL LRealT }
"string"            { mkL LStringT }

-- Keywords
"return"            { mkL LReturn }
"if"                { mkL LIf }
"else"              { mkL LElse }
"while"             { mkL LWhile }
"repeat"            { mkL LRepeat }
"case"              { mkL LCase }
"for"               { mkL LFor }
"in"                { mkL LIn }
":"                 { mkL LColon }
"."                 { mkL LDot }

-- Identifiers
@ident              { mkL LIdent }

-- Syntax things
".."                { mkL LRange }
"<=="               { mkL LNBTMove }
"><"                { mkL LSwap }
"->"                { mkL LGoesTo }
"="                 { mkL LGets }
","                 { mkL LComma }
"/"                 { mkL LCommand }
"("                 { mkL LLParenth }
")"                 { mkL LRParenth }
"["                 { mkL LLBracket }
"]"                 { mkL LRBracket }
"{"                 { mkL LLBrace }
"}"                 { mkL LRBrace }
"<"                 { mkL LLAngle }
">"                 { mkL LRAngle }
"|>"                { mkL LSeqStart }
"|-"                { mkL LSeqSeries  }
"|"                 { mkL LSeqCont }
"$"                 { mkL LConstant }
"~"                 { mkL LPure }
@partSeparator      { mkL LPartSeparator }
@blockSeparator     { mkL LBlockSeparator }
@blockStarter       { mkL LBlockStarter }

-- Expression Operators
"+"                 { mkL LPlus }
"-"                 { mkL LMinus }
"/"                 { mkL LDivide }
"%"                 { mkL LModulo }
"*"                 { mkL LTimes }
"/\"                { mkL LMax }
"\/"                { mkL LMin }
"&"                 { mkL LAnd }
"|"                 { mkL LOr }
"!"                 { mkL LNot }
"<"                 { mkL LLessThan }
"<="                { mkL LLessThanOrEqual }
">"                 { mkL LGreaterThan }
">="                { mkL LGreaterThanOrEqual }
"=="                { mkL LEqual }
"!="                { mkL LNotEqual }

-- Whitespace
\n                  ;

{

data LexemeClass = LInteger
                 | LBool
                 | LReal
                 | LString
                 | LIntT
                 | LBoolT
                 | LRealT
                 | LStringT
                 | LReturn
                 | LIf
                 | LElse
                 | LWhile
                 | LRepeat
                 | LCase
                 | LFor
                 | LIn
                 | LColon
                 | LDot
                 | LIdent
                 | LRange
                 | LNBTMove
                 | LSwap
                 | LGoesTo
                 | LGets
                 | LComma
                 | LCommand
                 | LLParenth
                 | LRParenth
                 | LLBracket
                 | LRBracket
                 | LLBrace
                 | LRBrace
                 | LLAngle
                 | LRAngle
                 | LSeqStart
                 | LSeqSeries
                 | LSeqCont
                 | LConstant
                 | LPure
                 | LPartSeparator
                 | LBlockSeparator
                 | LColon
                 | LPlus
                 | LMinus
                 | LDivide
                 | LModulo
                 | LTimes
                 | LMax
                 | LMin
                 | LAnd
                 | LOr
                 | LNot
                 | LLessThan
                 | LLessThanOrEqual
                 | LGreaterThan
                 | LGreaterThanOrEqual
                 | LEqual
                 | LNotEqual
    deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str in
                            case c of
                                LInteger            -> return (TInteger            ((read t) :: Integer) p)
                                LBool               -> return (TBool               (if t == "true" then True else False) p)
                                LReal               -> return (TReal               ((read t) :: Double) p)
                                LString             -> return (TString             (init $ tail t) p)
                                LIntT               -> return (TIntT               p)
                                LBoolT              -> return (TBoolT              p)
                                LRealT              -> return (TRealT              p)
                                LStringT            -> return (TStringT            p)
                                LReturn             -> return (TReturn             p)
                                LIf                 -> return (TIf                 p)
                                LElse               -> return (TElse               p)
                                LWhile              -> return (TWhile              p)
                                LRepeat             -> return (TRepeat             p)
                                LCase               -> return (TCase               p)
                                LFor                -> return (TFor                p)
                                LIn                 -> return (TIn                 p)
                                LColon              -> return (TColon              p)
                                LDot                -> return (TDot                p)
                                LIdent              -> return (TIdent              t p)
                                LRange              -> return (TRange              p)
                                LNBTMove            -> return (TNBTMove            p)
                                LSwap               -> return (TSwap               p)
                                LGoesTo             -> return (TGoesTo             p)
                                LGets               -> return (TGets               p)
                                LComma              -> return (TComma              p)
                                LCommand            -> return (TCommand            p)
                                LLParenth           -> return (TLParenth           p)
                                LRParenth           -> return (TRParenth           p)
                                LLBracket           -> return (TLBracket           p)
                                LRBracket           -> return (TRBracket           p)
                                LLBrace             -> return (TLBrace             p)
                                LRBrace             -> return (TRBrace             p)
                                LLAngle             -> return (TLAngle             p)
                                LRAngle             -> return (TRAngle             p)
                                LSeqStart           -> return (TSeqStart           p)
                                LSeqSeries          -> return (TSeqSeries          p)
                                LSeqCont            -> return (TSeqCont            p)
                                LConstant           -> return (TConstant           p)
                                LPure               -> return (TPure               p)
                                LPartSeparator      -> return (TPartSeparator      p)
                                LBlockSeparator     -> return (TBlockSeparator     p)
                                LColon              -> return (TColon              p)
                                LPlus               -> return (TPlus               p)
                                LMinus              -> return (TMinus              p)
                                LDivide             -> return (TDivide             p)
                                LModulo             -> return (TModulo             p)
                                LTimes              -> return (TTimes              p)
                                LMax                -> return (TMax                p)
                                LMin                -> return (TMin                p)
                                LAnd                -> return (TAnd                p)
                                LOr                 -> return (TOr                 p)
                                LNot                -> return (TNot                p)
                                LLessThan           -> return (TLessThan           p)
                                LLessThanOrEqual    -> return (TLessThanOrEqual    p)
                                LGreaterThan        -> return (TGreaterThan        p)
                                LGreaterThanOrEqual -> return (TGreaterThanOrEqual p)
                                LEqual              -> return (TEqual              p)
                                LNotEqual           -> return (TNotEqual           p)

alexEOF :: Alex Token
alexEOF = return TEoF

-- | Wrapper function for the lexer---allows the monadic lexer to be used with
-- a monadic parser
lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

-- | Type to represent tokens in the output stream
data Token = TInteger            { intVal :: Integer,       position :: AlexPosn } -- ^ An integral literal
           | TBool               { isTrue :: Bool,          position :: AlexPosn } -- ^ A boolean literal
           | TReal               { realVal :: Double,       position :: AlexPosn } -- ^ A real/floating-point literal
           | TString             { stringVal :: String,     position :: AlexPosn } -- ^ A string literal
           | TIntT               {                          position :: AlexPosn }
           | TBoolT              {                          position :: AlexPosn }
           | TRealT              {                          position :: AlexPosn }
           | TStringT            {                          position :: AlexPosn }
           | TReturn             {                          position :: AlexPosn }
           | TIf                 {                          position :: AlexPosn }
           | TElse               {                          position :: AlexPosn }
           | TWhile              {                          position :: AlexPosn }
           | TRepeat             {                          position :: AlexPosn }
           | TCase               {                          position :: AlexPosn }
           | TFor                {                          position :: AlexPosn }
           | TIn                 {                          position :: AlexPosn }
           | TColon              {                          position :: AlexPosn }
           | TDot                {                          position :: AlexPosn }
           | TIdent              { identifierVal :: String, position :: AlexPosn } -- ^ An identifier
           | TRange              {                          position :: AlexPosn }
           | TNBTMove            {                          position :: AlexPosn }
           | TSwap               {                          position :: AlexPosn }
           | TGoesTo             {                          position :: AlexPosn }
           | TGets               {                          position :: AlexPosn }
           | TComma              {                          position :: AlexPosn }
           | TCommand            {                          position :: AlexPosn }
           | TLParenth           {                          position :: AlexPosn }
           | TRParenth           {                          position :: AlexPosn }
           | TLBracket           {                          position :: AlexPosn }
           | TRBracket           {                          position :: AlexPosn }
           | TLBrace             {                          position :: AlexPosn }
           | TRBrace             {                          position :: AlexPosn }
           | TLAngle             {                          position :: AlexPosn }
           | TRAngle             {                          position :: AlexPosn }
           | TSeqStart           {                          position :: AlexPosn }
           | TSeqSeries          {                          position :: AlexPosn }
           | TSeqCont            {                          position :: AlexPosn }
           | TConstant           {                          position :: AlexPosn }
           | TPure               {                          position :: AlexPosn }
           | TPartSeparator      {                          position :: AlexPosn }
           | TBlockSeparator     {                          position :: AlexPosn }
           | TColon              {                          position :: AlexPosn }
           | TPlus               {                          position :: AlexPosn }
           | TMinus              {                          position :: AlexPosn }
           | TDivide             {                          position :: AlexPosn }
           | TModulo             {                          position :: AlexPosn }
           | TTimes              {                          position :: AlexPosn }
           | TMax                {                          position :: AlexPosn }
           | TMin                {                          position :: AlexPosn }
           | TAnd                {                          position :: AlexPosn }
           | TOr                 {                          position :: AlexPosn }
           | TNot                {                          position :: AlexPosn }
           | TLessThan           {                          position :: AlexPosn }
           | TLessThanOrEqual    {                          position :: AlexPosn }
           | TGreaterThan        {                          position :: AlexPosn }
           | TGreaterThanOrEqual {                          position :: AlexPosn }
           | TEqual              {                          position :: AlexPosn }
           | TNotEqual           {                          position :: AlexPosn }
           | TEoF                                                                  -- ^ @\\0@
    deriving (Eq, Ord)

instance Show Token where
    show (TDocAssignmentLine   _) = "TDocAssignmentLine"
    show (TDocLine           c _) = "// " ++ c
    show (TInteger           i _) = show i
    show (TBool              b _) = show b
    show (TReal              r _) = show r
    show (TChar              c _) = show c
    show (TString            s _) = show s
    show (TIf                  _) = "if"
    show (TElse                _) = "else"
    show (TWhile               _) = "while"
    show (TRepeat              _) = "repeat"
    show (TWith                _) = "with"
    show (TSwitch              _) = "switch"
    show (TFor                 _) = "for"
    show (TImport              _) = "import"
    show (TModule              _) = "module"
    show (TIdent             s _) = show s
    show (TPartSeparator       _) = ";/\\n"
    show (TBlockSeparator      _) = "#"
    show (TQueue               _) = "<-"
    show (TGoesTo              _) = "->"
    show (TGets                _) = "="
    show (TLParenth            _) = "("
    show (TRParenth            _) = ")"
    show (TLBracket            _) = "["
    show (TRBracket            _) = "]"
    show (TLBrace              _) = "{"
    show (TRBrace              _) = "}"
    show (TImpure              _) = "@"
    show (TPlus                _) = "+"
    show (TMinus               _) = "-"
    show (TDivide              _) = "/"
    show (TModulo              _) = "%"
    show (TTimes               _) = "*"
    show (TShiftLeft           _) = "<<"
    show (TShiftRight          _) = ">>"
    show (TShiftRightSameSign  _) = ">>>"
    show (TAndScrict           _) = "&"
    show (TAndLazy             _) = "&&"
    show (TOrStrict            _) = "|"
    show (TOrLazy              _) = "||"
    show (TNot                 _) = "!"
    show (TXor                 _) = "^"
    show (TLessThan            _) = "<"
    show (TLessThanOrEqual     _) = "<="
    show (TGreaterThan         _) = ">"
    show (TGreaterThanOrEqual  _) = ">="
    show (TImplies             _) = "=>"
    show (TEqual               _) = "=="
    show (TNotEqual            _) = "!="
    show (TComma               _) = ","
    show (TIntT                _) = "int"
    show (TBoolT               _) = "bool"
    show (TRealT               _) = "real"
    show (TCharT               _) = "char"
    show (TStringT               _) = "string"
    show (TUnit                _) = "Unit"
    show (TAnyT                _) = "Any"
    show (TColon               _) = ":"
    show (TIsSubType           _) = "<:"
    show (TIsImplementeBy      _) = "<~"
    show (TIsType              _) = "|>"
    show (TClass               _) = "class"
    show (TComponent           _) = "component"
    show (TIDC                 _) = "_"
    show (TReturn              _) = "return"
    show TEoF                     = "EoF"

-- | AlexPosn is ordered by the total number of characters read (its final field)
instance Ord AlexPosn where
    (AlexPn c1 _ _ ) < (AlexPn c2 _ _) = c1 < c2
    a <= b = (a < b) || (a == b)

instance ToJSON AlexPosn where
    toJSON (AlexPn a b c) = object ["total" .= a, "line" .= b, "char" .= c]

instance FromJSON AlexPosn where
    parseJSON (Object v) = AlexPn <$> v .: "total" <*> v .: "line" <*> v .: "char"
    parseJSON _ = fail "Expected object when parsing position datum"

}
