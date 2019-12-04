{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ScopeResolver.ScopeResolver (resolveScope, SymTable) where

import           Args            (Args)
import           Data.Map.Lazy   (Map)
import           GHC.Generics
import           Language.AST    (AST (..), Assignment (..), BodyBlock (..),
                                  BodyLine (..), Call (..), Command (..),
                                  CondBlock, Else, Event (..), Expr (..),
                                  FunctionBodyBlock (..), FunctionDef (..),
                                  FunctionSignature (..), Ident (..),
                                  Modifier (..), ModuleItem (..), NBTMove (..),
                                  RangeDef (..), Sequence (..), SpadeType (..),
                                  SwitchCase (..), Value (..))
import           Results.Results (Result (..))

type NextUnique = Int
type SymTable = Map Ident Int
type ScopeStack = [(String, Int)]

resolveScope :: Args -> AST -> Result (SymTable,AST)
resolveScope _ a = case scope [] mempty a of
    Pass (_, _, st, a') ws -> Pass (st, a') ws
    Fail m                 -> Fail m

class Scopeable a where
    scope :: NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, ScopeStack, SymTable, a)

    default scope :: (Generic a, GScopeable (Rep a)) => NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, ScopeStack, SymTable, a)
    scope ss st = (gScope ss st) . from

class GScopeable f where
    gScope :: NextUnique -> ScopeStack -> SymTable -> f a -> Result (NextUnique, ScopeStack, SymTable, f a)

instance GScopeable U1 where
    gScope ss st u = Pass $ (ss, st, u)

instance (GScopeable a, GScopeable b) => GScopeable (a :*: b) where
    gScope n ss st (x :*: y) = case gScope n ss st x of
        Pass (n', _, st', x') ws1 -> case gScope n ss st y of
            Pass (n'', _, st'', y') ws2 -> Pass (n'', ss, st'', x :*: y)
            Fail x                      -> Fail x
        Fail x -> Fail x

instance (GScopeable a, GScopeable b) => GScopeable (a :+: b) where
    gScope n ss st (L1 x) = gScope n ss st x
    gScope n ss st (R1 y) = gScope n ss st y

instance GScopeable a => GScopeable (M1 i c a) where
    gScope ss st (M1 x) = gScope ss st x

instance Scopeable a => GScopeable (K1 i a) where
    gScope ss st (K1 x) = scope ss st x

instance Scopeable AST
-- instance Scopeable Assignment
-- instance Scopeable BodyBlock
instance Scopeable BodyLine
instance Scopeable Call
instance Scopeable Command
deriving instance Scopeable CondBlock
instance Scopeable Else
instance Scopeable Event
instance Scopeable Expr
instance Scopeable FunctionBodyBlock
instance Scopeable FunctionDef
instance Scopeable FunctionSignature
-- instance Scopeable Ident
instance Scopeable Modifier
instance Scopeable ModuleItem
instance Scopeable NBTMove
instance Scopeable RangeDef
instance Scopeable Sequence
instance Scopeable SpadeType
instance Scopeable SwitchCase
instance Scopeable Value

-- sequenceScope :: Scopeable a => ScopeStack -> SymTable -> [a] -> Result (ScopeStack, SymTable, [a])
-- sequenceScope ss st [] = Pass (ss, st, []) []
-- sequenceScope ss st (a:as) = case scope ss st a of
--     Pass (ss', st', a') ws -> case sequenceScope ss' st' as of
--         Pass (ss'', st'', as') ws' -> Pass (ss'', st'', a':as') (ws <> ws')
--         x                          -> x
--     Fail m -> Fail m
