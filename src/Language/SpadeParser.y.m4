-- M4 definitions
include(`m4/defs.m4')

{
{-|
Module      : SpadeParser
Description : Parser for Spade
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines the machinery to parse Spade from a token stream generated by the lexer.
-}
module Language.SpadeParser (parseSpade) where

import Language.AST
import Language.SpadeLexer (Alex, AlexPosn(..), Token(..), lexWrap, alexError, runAlex)
import Language.Position (GetPos, getPos)

}

%name parseSpade ast
%name parseREPL moduleItem

%error { parseError }
%lexer { lexWrap } { TEoF }
%monad { Alex }
%tokentype { Token }

-- Enforce perfection
-- %expect 0

%token
    BOOL            { TBool                 isTrue p }
    IDENT           { TIdent                identifierVal p }
    COMMAND_PART    { TCommandPart          commandPartVal p }
    INT             { TInteger              intVal p }
    REAL            { TReal                 realVal p }
    STRING          { TString               stringVal p }
    LINE_COMMENT    { TLineComment          p }
    ":"             { TColon                p }
    "}"             { TBlockSeparator       p }
    "{"             { TBlockStarter         p }
    "\n"            { TPartSeparator        p }
    "("             { TLParenth             p }
    ")"             { TRParenth             p }
    "["             { TLBracket             p }
    "]"             { TRBracket             p }
    ".."            { TRange                p }
    "..."           { TListCont             p }
    ","             { TComma                p }
    "."             { TDot                  p }
    "|>"            { TSeqStart             p }
    "bool"          { TBoolT                p }
    "byte"          { TByteT                p }
    "short"         { TShortT               p }
    "int"           { TIntT                 p }
    "long"          { TLongT                p }
    "float"         { TFloatT               p }
    "real"          { TRealT                p }
    "string"        { TStringT              p }
    "range"         { TRangeT               p }
    "case"          { TCase                 p }
    "else"          { TElse                 p }
    "if"            { TIf                   p }
    "for"           { TFor                  p }
    "in"            { TIn                   p }
    "repeat"        { TRepeat               p }
    "while"         { TWhile                p }
    "return"        { TReturn               p }
    "="             { TGets                 p }
    "<-"            { TNBTMove              p }
    "><"            { TSwap                 p }
    "$"             { TDollar               p }
    "=="            { TEqual                p }
    "!="            { TNotEqual             p }
    "<"             { TLAngle               p }
    "<="            { TLessThanOrEqual      p }
    ">="            { TGreaterThanOrEqual   p }
    ">"             { TRAngle               p }
    "/\\"           { TMax                  p }
    "\\/"           { TMin                  p }
    "&"             { TAnd                  p }
    "!"             { TNot                  p }
    "|"             { TOr                   p }
    "/"             { TForwardSlash         p }
    "+"             { TPlus                 p }
    "-"             { TMinus                p }
    "*"             { TTimes                p }
    "%"             { TModulo               p }

%left CALL
%right "->"
%left "|"
%left "&"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%right NEG "!"
%right "$" "~"
%nonassoc "{" "[" "(" INT REAL CHAR BOOL IDENT STRING

%%

ast :: {AST}
ast : moduleItems   { AST $1 }

list1(moduleItems, ModuleItem, moduleItem, "\n")
-- moduleItems :: {[ModuleItem]}
-- moduleItems : moduleItem                   { [$1] }
--             | moduleItem "\n" moduleItems  { $1 : $3 }

moduleItem :: {ModuleItem}
moduleItem : functionDef    { FunctionItem $1 (getPos $1) }

functionDef :: {FunctionDef}
functionDef : functionSignature "{" functionBodyBlocks "}" { FunctionDef $1 $3 (getPos $1) }

functionSignature :: {FunctionSignature}
functionSignature : IDENT "(" typedIdents ")" ":" spadeType { FunctionSignature (Ident (identifierVal $1) (getPos $1)) $3 $6 (getPos $1)  }
                  | IDENT "(" typedIdents ")" { FunctionSignature (Ident (identifierVal $1) (getPos $1)) $3 (Void (getPos $1)) (getPos $1) }

list1(functionBodyBlocks, FunctionBodyBlock, functionBody, "\n")

functionBody :: {FunctionBodyBlock}
functionBody : "|>" sequenceBodies { SequenceBody $2 (getPos $1) }
             | bodyBlock           { BodyBody $1 (getPos $1) }

sequenceBodies :: {[Sequence]}
sequenceBodies : sequenceBody                { [$1] }
               | sequenceBody sequenceBodies { $1 : $2 }

sequenceBody :: {Sequence}
sequenceBody : event "{" bodyBlocks "}" { Sequence $1 $3 (getPos $1) }

event :: {Event}
event : expr { Event $1 (getPos $1) }

condBlocks :: {[CondBlock]}
condBlocks : condBlock                        { [$1] }
           | condBlock "else" "if" condBlocks { $1 : $4 }

condBlock :: {CondBlock}
condBlock : expr bodyBlocks { ($1, $2) }

list1(switchCases, SwitchCase, switchCase, "\n")

switchCase :: {SwitchCase}
switchCase : expr ":" "{" bodyBlocks "}" { SwitchCase $1 $4 (getPos $1) }

list(bodyBlocks, BodyBlock, bodyBlock, "\n")

bodyBlock :: {BodyBlock}
bodyBlock : bodyLine                                  { Line $1 (getPos $1) }
          | "if" condBlocks                           { If $2 Nothing (getPos $1) }
          | "if" condBlocks "else" "{" bodyBlocks "}" { If $2 (Just $5) (getPos $1) }
          | "while" expr "{" bodyBlocks "}"           { While $2 $4 (getPos $1) }
          | "for" IDENT "in" expr "{" bodyBlocks "}"  { For (Ident (identifierVal $2) (getPos $2)) $4 $6 (getPos $1) }
          | "repeat" expr "{" bodyBlocks "}"          { Repeat $2 $4 (getPos $1) }
          | "case" expr "{" switchCases "}"           { Switch $2 $4 (getPos $1) }

bodyLine :: {BodyLine}
bodyLine : IDENT "=" expr         { AssignmentC (Assignment (Ident (identifierVal $1) (getPos $1)) $3 (getPos $1)) }
         | expr "<-" expr         { NBTMoveC (NBTMove $1 $3 (getPos $1)) }
         | "/" command            { CommandC (command $2) }
         | IDENT "(" exprList ")" { CallC (Call (Ident (identifierVal $1) (getPos $1)) $3 (Unknown (getPurity (identifierVal $1)) (getPos $1)) (getPos $1)) }
         | "return"               { Return Nothing (getPos $1) }
         | "return" expr          { Return (Just $2) (getPos $1) }

include(`MCFunctionParser.y.m4')

list(exprList, Expr, expr, `","')

list(exprMap, `(Expr, Expr)', exprMapPart, `","')

exprMapPart :: {(Expr, Expr)}
exprMapPart : expr ":" expr { ($1, $3) }

expr :: {Expr}
expr : value                   { Value $1 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "-" expr                { Neg $2 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "+" expr           { Add $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "-" expr           { Subtract $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "*" expr           { Multiply $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "/" expr           { Divide $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "%" expr           { Modulo $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "<" expr           { Less $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "<=" expr          { LessOrEqual $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr ">" expr           { Greater $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr ">=" expr          { GreaterOrEqual $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "==" expr          { Equal $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "!=" expr          { NotEqual $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "/\\" expr         { Max $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "\\/" expr         { Min $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "!" expr                { Not $2 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "&" expr           { And $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | expr "|" expr           { Or $1 $3 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "[" exprList "]"        { List $2 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "{" exprMap "}"         { Map $2 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "[" expr ".." expr "]"  { Range (ClosedRange $2 $4 (Unknown UnknownM (getPos $1)) (getPos $1)) }
     | "[" ".." expr "]"       { Range (LeftOpenRange $3 (Unknown UnknownM (getPos $1)) (getPos $1)) }
     | "[" expr ".." "]"       { Range (RightOpenRange $2 (Unknown UnknownM (getPos $1)) (getPos $1)) }
     | "[" expr "..." expr "]" { ListCont $2 $4 (Unknown UnknownM (getPos $1)) (getPos $1) }
     | "<" spadeType ">" expr  { TypeCast $4 $2 (getPos $1) }
     | "(" expr ")"            { Brackets $2 (getPos $1) }

value :: {Value}
value : INT                    { IntegerV (intVal $1) (Unknown UnknownM (getPos $1)) (getPos $1) }
      | REAL                   { DoubleV (realVal $1) (Unknown UnknownM (getPos $1)) (getPos $1) }
      | STRING                 { StringV (stringVal $1) (Unknown UnknownM (getPos $1)) (getPos $1) }
      | IDENT                  { IdentV (Ident (identifierVal $1) (getPos $1)) (Unknown UnknownM (getPos $1)) (getPos $1) }
      | BOOL                   { BoolV (isTrue $1) (Unknown UnknownM (getPos $1)) (getPos $1) }
      | IDENT "(" exprList ")" { CallV (Call (Ident (identifierVal $1) (getPos $1)) $3 (Unknown (getPurity (identifierVal $1)) (getPos $1)) (getPos $1)) (Unknown (getPurity (identifierVal $1)) (getPos $1)) (getPos $1) }

spadeType :: {SpadeType}
spadeType : "bool"              { BoolT UnknownM (getPos $1) }
          | "byte"              { ByteT UnknownM (getPos $1) }
          | "short"             { ShortT UnknownM (getPos $1) }
          | "int"               { IntegerT UnknownM (getPos $1) }
          | "long"              { LongT UnknownM (getPos $1) }
          | "float"             { FloatT UnknownM (getPos $1) }
          | "real"              { DoubleT UnknownM (getPos $1) }
          | "string"            { StringT UnknownM (getPos $1) }
          | "range"             { RangeT UnknownM (getPos $1) }
          | "[" spadeType "]"   { ListT $2 UnknownM (getPos $1) }
          | "{" typedIdents "}" { MapT $2 UnknownM (getPos $1) }

list(typedIdents, `(String, SpadeType)', typedIdent, `","')

typedIdent :: {(String, SpadeType)}
typedIdent : IDENT ":" spadeType { ((identifierVal $1) (getPos $1), $3) }

{

parseError :: Token -> Alex a
parseError t = case t of
    TEoF -> alexError $ "Unexpected EoF"
    t' -> case position t' of
        AlexPn _ l c -> alexError $ show l ++ ":" ++ show c ++ ": " ++ "Parse error on " ++ show t

getPurity :: String -> Modifier
getPurity ('$':_) = Prep
getPurity ('~':_) = Pure
getPurity _       = Impure

}
