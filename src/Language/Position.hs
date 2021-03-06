{-|
Module      : Position
Description : Positional information for the AST
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the machinery to parse the Emperor language from a token stream generated by the Emperor lexer.
-}
module Language.Position
    ( GetPos
    , getPos
    ) where

import           Language.AST
import           Language.SpadeLexer (AlexPosn (..), Token, position)

class GetPos a where
    getPos :: a -> AlexPosn -- ^ Return the position in the input stream

instance GetPos a => GetPos [a] where
    getPos (x:_) = getPos x
    getPos []    = error "Attempted to get position of empty list"

instance GetPos AST where
    getPos (AST (x:_)) = getPos x
    getPos (AST _)     = error "Attempted to get position from empty AST"

instance GetPos ModuleItem where
    getPos (FunctionItem _ p) = p

instance GetPos FunctionDef where
    getPos (FunctionDef _ _ p) = p

instance GetPos FunctionSignature where
    getPos (FunctionSignature _ _ _ p) = p

instance GetPos FunctionBodyBlock where
    getPos (SequenceBody _ p) = p
    getPos (BodyBody _ p)     = p

instance GetPos Sequence where
    getPos (Sequence _ _ p) = p

instance GetPos Event where
    getPos (Event _ p) = p

instance GetPos BodyBlock where
    getPos (Line _ p)     = p
    getPos (If _ _ p)     = p
    getPos (While _ _ p)  = p
    getPos (For _ _ _ p)  = p
    getPos (Repeat _ _ p) = p
    getPos (Switch _ _ p) = p

instance GetPos SwitchCase where
    getPos (SwitchCase _ _ p) = p


instance GetPos BodyLine where
    getPos (AssignmentC a) = getPos a
    getPos (NBTMoveC m)    = getPos m
    getPos (SwapC _ _ p)   = p
    getPos (CommandC c)    = getPos c
    getPos (CallC c)       = getPos c
    getPos (Return _ p)    = p

instance GetPos Assignment where
    getPos (Assignment _ _ p) = p

instance GetPos Command where
    getPos (Command _ _ p) = p

instance GetPos Expr where
    getPos (Value _ _ p)            = p
    getPos (Neg _ _ p)              = p
    getPos (Add _ _ _ p)            = p
    getPos (Subtract _ _ _ p)       = p
    getPos (Multiply _ _ _ p)       = p
    getPos (Divide _ _ _ p)         = p
    getPos (Modulo _ _ _ p)         = p
    getPos (Less _ _ _ p)           = p
    getPos (LessOrEqual _ _ _ p)    = p
    getPos (Greater _ _ _ p)        = p
    getPos (GreaterOrEqual _ _ _ p) = p
    getPos (Equal _ _ _ p)          = p
    getPos (NotEqual _ _ _ p)       = p
    getPos (Max _ _ _ p)            = p
    getPos (Min _ _ _ p)            = p
    getPos (Not _ _ p)              = p
    getPos (And _ _ _ p)            = p
    getPos (Or _ _ _ p)             = p
    getPos (MemberOf _ _ _ p)       = p
    getPos (Subscript _ _ _ p)      = p
    getPos (List _ _ p)             = p
    getPos (Map _ _ p)              = p
    getPos (Range d)                = getPos d
    getPos (ListCont _ _ _ p)       = p
    getPos (TypeCast _ _ p)         = p
    getPos (Brackets _ p)           = p

instance GetPos RangeDef where
    getPos (ClosedRange _ _ _ p)  = p
    getPos (LeftOpenRange _ _ p)  = p
    getPos (RightOpenRange _ _ p) = p

instance GetPos Value where
    getPos (IntegerV _ _ p) = p
    getPos (DoubleV _ _ p)  = p
    getPos (StringV _ _ p)  = p
    getPos (IdentV _ _ p)   = p
    getPos (BoolV _ _ p)    = p
    getPos (CallV _ _ p)    = p
    getPos (ByteV _ _ p)    = p
    getPos (ShortV _ _ p)   = p
    getPos (LongV _ _ p)    = p
    getPos (FloatV _ _ p)   = p

instance GetPos Call where
    getPos (Call _ _ _ p) = p

instance GetPos Ident where
    getPos (Ident _ p) = p

instance GetPos NBTMove where
    getPos (NBTMove _ _ p) = p

instance GetPos Token where
    getPos = position

