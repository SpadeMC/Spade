{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AST
Description : Data-structures for the abstract syntax tree
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines the data-types used to form the abstract syntax tree of the
spade language.
-}
module Parser.AST where

import Language.SpadeLexer (AlexPosn(..))
import Types.Results (SpadeType(..), Purity(..))

-- | Data type to represent the abstract syntax tree for a single module. This is specified by its name, its imports and its code.
data AST =
    AST [ModuleItem]
    deriving (Show)

-- | Describes a single named item in the module
data ModuleItem = FunctionItem FunctionDef AlexPosn
    deriving (Show)

-- | Describes the definition of a function
data FunctionDef =
    FunctionDef FunctionSignature [BodyBlock] AlexPosn
    deriving (Show)

-- | Describes the definition of the type of a function
data FunctionSignature =
    FunctionSignature Ident [(Ident, SpadeType)] SpadeType AlexPosn
    deriving (Show)

-- | Represents a single construction in the body of a function. This may be a
-- further construct or just a single line
data BodyBlock
    = Line BodyLine AlexPosn -- ^ A single line of code
    | IfElse Expr [BodyBlock] [BodyBlock] AlexPosn -- ^ An if-else block
    | While Expr [BodyBlock] AlexPosn -- ^ A while-loop
    | For Ident Expr [BodyBlock] AlexPosn -- ^ A for-loop
    | Repeat Expr [BodyBlock] AlexPosn -- ^ A repeat-loop
    | Switch Expr [SwitchCase] AlexPosn -- ^ A switch-case statement
    | Sequence [(Number, BodyBlock)] AlexPosn -- ^ A sequence definition
    deriving (Show)

-- | Data-struecture for the switch-case statement
data SwitchCase =
    SwitchCase Expr [BodyBlock] AlexPosn
    deriving (Show)

-- | Data-structure for a single body-line
data BodyLine
    = AssignmentC Assignment
    | NBTMoveC NBTMove
    | CommandC Command
    | CallC Call
    | Return (Maybe Expr) AlexPosn
    deriving (Show)

-- | Data-structure to represent an assignment statement
data Assignment =
    Assignment (Maybe EmperorType) Ident Expr AlexPosn
    deriving (Show)

-- | Data-structure to represent an expression
data Expr
    = Value Value AlexPosn
    | Neg Expr AlexPosn
    | Add Expr Expr AlexPosn
    | Subtract Expr Expr AlexPosn
    | Multiply Expr Expr AlexPosn
    | Divide Expr Expr AlexPosn
    | Modulo Expr Expr AlexPosn
    | Less Expr Expr AlexPosn
    | LessOrEqual Expr Expr AlexPosn
    | Greater Expr Expr AlexPosn
    | GreaterOrEqual Expr Expr AlexPosn
    | Equal Expr Expr AlexPosn
    | NotEqual Expr Expr AlexPosn
    | Not Expr AlexPosn
    | And Expr Expr AlexPosn
    | Or Expr Expr AlexPosn
    | Set [Expr] AlexPosn
    | Tuple [Expr] AlexPosn
    | List [Expr] AlexPosn
    deriving (Show)

-- | Data-structure to represent a single value
data Value
    = IDC AlexPosn
    | Integer Integer AlexPosn
    | Real Double AlexPosn
    | StringV String AlexPosn
    | IdentV Ident AlexPosn
    | Bool Bool AlexPosn
    | CallV Call AlexPosn
    deriving (Show)

-- | Represents the use of a function
data Call =
    Call Purity Ident [Expr] AlexPosn
    deriving (Show)

-- | Data-structure to represent an identifier
data Ident =
    Ident String AlexPosn
    deriving (Eq, Ord, Show)

-- | NBT Move operation
data NBTMove =
    String AlexPosn
    deriving (Show)
