{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ScopeResolver.ScopeResolver (resolveScope, SymTable) where

import           Args            (Args)
import           Data.Map.Lazy   (Map)
import           GHC.Generics
import           Language.AST    (AST, Ident (..), ModuleItem)
import           Results.Results (Result (..))

type NextUnique = Int
type SymTable = Map Ident Int
type ScopeStack = [(String, Int)]

resolveScope :: Args -> AST -> Result (SymTable, AST)
resolveScope _ a = case scope 0 mempty mempty a of
    Pass (_, st) ws -> Pass (st, a) ws
    Fail m          -> Fail m

class Scope a where
    scope :: NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, SymTable)

    default scope :: (Generic a, GScope (Rep a)) => NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, SymTable)
    scope n ss st = (gScope n ss st) . from

class GScope f where
    gScope :: NextUnique -> ScopeStack -> SymTable -> f a -> Result (NextUnique, SymTable)

instance GScope V1 where
    gScope _ _ _ _ = undefined

-- Leaves
instance GScope U1 where
    gScope n _ st _ = Pass (n, st) []

-- Out-degree 1, container
instance GScope a => GScope (K1 t (a p)) where
    gScope n ss st (K1 x) = gScope n ss st x

-- instance (GScope (K1 R ModuleItem)) where
--     gScope n ss st = (scope n ss st) . from
instance Scope ModuleItem
instance GScope [] where -- This shouldn't be necessary??
    gScope n _ st [] = Pass (n, st) []
    -- gScope n ss st (x:xs) = case gScope n ss st x of
    --     Pass (n', st') ws -> case gScope n' ss st' xs of
    --         Pass (n'', st'') ws' -> Pass (n'', st'') (ws <> ws')
    --         Fail m               -> Fail m
    --     Fail m -> Fail m

-- Out-degree 1, wrapper
instance GScope a => GScope (M1 i c a) where
    gScope n ss st (M1 x) = gScope n ss st x

-- Out-degree 2, union
instance (GScope a, GScope b) => GScope (a :+: b) where
    gScope n ss st (L1 a) = gScope n ss st a
    gScope n ss st (R1 b) = gScope n ss st b

-- Out-degree 2, intersection
instance (GScope a, GScope b) => GScope (a :*: b) where
    gScope n ss st (a :*: b) = case gScope n ss st a of
        Pass (n', st') ws -> case gScope n' ss st' b of
            Pass (n'', st'') ws' -> Pass (n'', st'') $ (ws <> ws')
            Fail m               -> Fail m
        Fail m            -> Fail m

instance (Generic a) => GScope (f a)
instance (Generic a, GScope (Rep a)) => Scope a
-- instance (GScope (K1 R a)) => Scope a

-- instance GScope a => GScope (M1 i c a) where
--     gScope (M1 a) = gScope a

-- instance GScope a => GScope (K1 i a)
-- resolveScope :: Args -> AST -> Result (SymTable,AST)
-- resolveScope _ a = case scope 0 mempty mempty a of
--     Pass (_, _, st, a') ws -> Pass (st, a') ws
--     Fail m                 -> Fail m

-- class Scopeable a where
--     scope :: NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, ScopeStack, SymTable, a)

--     default scope :: (Generic a, GScopeable (Rep a)) => NextUnique -> ScopeStack -> SymTable -> a -> Result (NextUnique, ScopeStack, SymTable, a)
--     scope n ss st = (gScope n ss st) . from

-- class GScopeable f where
--     gScope :: NextUnique -> ScopeStack -> SymTable -> f a -> Result (NextUnique, ScopeStack, SymTable, f a)

-- instance GScopeable U1 where
--     gScope n ss st u = Pass (n, ss, st, u) []

-- instance (GScopeable a, GScopeable b) => GScopeable (a :*: b) where
--     gScope n ss st (x :*: y) = case gScope n ss st x of
--         Pass (n', ss', st', x') ws1 -> case gScope n ss' st' y of
--             Pass (n'', ss'', st'', y') ws2 -> Pass (n'', ss'', st'', x :*: y) (ws1 <> ws2)
--             Fail x                         -> Fail x
--         Fail x -> Fail x

-- instance (GScopeable a, GScopeable b) => GScopeable (a :+: b) where
--     gScope n ss st (L1 x) = (n', ss', st', L1 x')
--         where
--             (n', ss', st', x') = gScope n ss st x
--     gScope n ss st (R1 y) = (n', ss', st', R1 y')
--         where
--             (n', ss', st', y') = gScope n ss st y

-- instance GScopeable a => GScopeable (M1 i c a) where
--     gScope n ss st (M1 x) = gScope n ss st x

-- instance Scopeable a => GScopeable (K1 i a) where
--     gScope n ss st (K1 x) = scope n ss st x

-- instance Scope AST
-- instance Scopeable Assignment
-- instance Scopeable BodyBlock
-- instance Scope BodyLine
-- instance Scope Call
-- instance Scope Command
-- deriving instance Scope CondBlock
-- instance Scope Else
-- instance Scope Event
-- instance Scope Expr
-- instance Scope FunctionBodyBlock
-- instance Scope FunctionDef
-- instance Scope FunctionSignature
-- -- instance Scopeable Ident
-- instance Scope Modifier
-- instance Scope ModuleItem
-- instance Scope NBTMove
-- instance Scope RangeDef
-- instance Scope Sequence
-- instance Scope SpadeType
-- instance Scope SwitchCase
-- instance Scope Value

-- -- sequenceScope :: Scopeable a => ScopeStack -> SymTable -> [a] -> Result (ScopeStack, SymTable, [a])
-- -- sequenceScope ss st [] = Pass (ss, st, []) []
-- -- sequenceScope ss st (a:as) = case scope ss st a of
-- --     Pass (ss', st', a') ws -> case sequenceScope ss' st' as of
-- --         Pass (ss'', st'', as') ws' -> Pass (ss'', st'', a':as') (ws <> ws')
-- --         x                          -> x
-- --     Fail m -> Fail m
