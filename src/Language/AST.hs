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
module Language.AST where

import           Data.Int
import           Language.SpadeLexer (AlexPosn (..))
-- import Types.Results (SpadeType(..), Purity(..))

-- | Data type to represent the abstract syntax tree for a single module. This is specified by its name, its imports and its code.
data AST =
    AST [ModuleItem]
    deriving (Show)

-- | Describes a single named item in the module
data ModuleItem = FunctionItem FunctionDef AlexPosn
    deriving (Show)

-- | Describes the definition of a function
data FunctionDef =
    FunctionDef FunctionSignature [FunctionBodyBlock] AlexPosn
    deriving (Show)

-- | Describes the definition of the type of a function
data FunctionSignature =
    FunctionSignature Ident [(Ident, SpadeType)] SpadeType AlexPosn
    deriving (Show)

-- | Describes the definition of blocks or sequences
data FunctionBodyBlock
    = SequenceBody [Sequence] AlexPosn
    | BodyBody BodyBlock AlexPosn
    deriving (Show)

-- | Describes the definition of a sequence of events
data Sequence
    = Sequence Event [BodyBlock] AlexPosn
    deriving (Show)

data Event = Event Expr AlexPosn
    deriving (Show)

-- | Represents a single construction in the body of a function. This may be a
-- further construct or just a single line
data BodyBlock
    = Line BodyLine AlexPosn -- ^ A single line of code
    | If [CondBlock] (Maybe Else) AlexPosn -- ^ An if-else block
    | While Expr [BodyBlock] AlexPosn -- ^ A while-loop
    | For Ident Expr [BodyBlock] AlexPosn -- ^ A for-loop
    | Repeat Expr [BodyBlock] AlexPosn -- ^ A repeat-loop
    | Switch Expr [SwitchCase] AlexPosn -- ^ A switch-case statement
    deriving (Show)

type CondBlock = (Expr, [BodyBlock])

type Else = [BodyBlock]

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
    Assignment Ident Expr AlexPosn
    deriving (Show)

data Command =
    Command [Either String Expr] SpadeType AlexPosn
    deriving (Show)

-- | Data-structure to represent an expression
data Expr
    = Value Value SpadeType AlexPosn
    | Neg Expr SpadeType AlexPosn
    | Add Expr Expr SpadeType AlexPosn
    | Subtract Expr Expr SpadeType AlexPosn
    | Multiply Expr Expr SpadeType AlexPosn
    | Divide Expr Expr SpadeType AlexPosn
    | Modulo Expr Expr SpadeType AlexPosn
    | Less Expr Expr SpadeType AlexPosn
    | LessOrEqual Expr Expr SpadeType AlexPosn
    | Greater Expr Expr SpadeType AlexPosn
    | GreaterOrEqual Expr Expr SpadeType AlexPosn
    | Equal Expr Expr SpadeType AlexPosn
    | NotEqual Expr Expr SpadeType AlexPosn
    | Max Expr Expr SpadeType AlexPosn
    | Min Expr Expr SpadeType AlexPosn
    | Not Expr SpadeType AlexPosn
    | And Expr Expr SpadeType AlexPosn
    | Or Expr Expr SpadeType AlexPosn
    | List [Expr] SpadeType AlexPosn
    | Map [(Expr, Expr)] SpadeType AlexPosn
    | Range RangeDef
    | ListCont Expr Expr SpadeType AlexPosn
    | TypeCast Expr SpadeType AlexPosn
    | Brackets Expr AlexPosn
    deriving (Show)

data RangeDef
    = ClosedRange Expr Expr SpadeType AlexPosn
    | LeftOpenRange Expr SpadeType AlexPosn
    | RightOpenRange Expr SpadeType AlexPosn
    deriving (Show)

-- | Data-structure to represent a single value
data Value
    = IntegerV Integer SpadeType AlexPosn
    | DoubleV Double SpadeType AlexPosn
    | StringV String SpadeType AlexPosn
    | IdentV Ident SpadeType AlexPosn
    | BoolV Bool SpadeType AlexPosn
    | CallV Call SpadeType AlexPosn
    | ByteV Int8 SpadeType AlexPosn
    | ShortV Int16 SpadeType AlexPosn
    | LongV Int64 SpadeType AlexPosn
    | FloatV Float SpadeType AlexPosn
    deriving (Show)

-- | Represents the use of a function
data Call =
    Call Ident [Expr] SpadeType AlexPosn
    deriving (Show)

data Modifier
    = Pre AlexPosn
    | Pure AlexPosn
    | Impure AlexPosn
    deriving (Show)

-- | Data-structure to represent an identifier
data Ident =
    Ident String AlexPosn
    deriving (Eq, Ord, Show)

-- | NBT Move operation
data NBTMove =
    NBTMove Expr Expr AlexPosn
    deriving (Show)

data SpadeType
    = Unknown AlexPosn
    | Void AlexPosn
    | IntegerT AlexPosn
    | DoubleT AlexPosn
    | StringT AlexPosn
    | IdentVT AlexPosn
    | BoolT AlexPosn
    | ByteT AlexPosn
    | ShortT AlexPosn
    | LongT AlexPosn
    | FloatT AlexPosn
    | RangeT AlexPosn
    | ListT SpadeType AlexPosn
    | MapT [(Ident,SpadeType)] AlexPosn
    deriving (Show)
