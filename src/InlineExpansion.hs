{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : InlineExpansion
Description : Optimizacion de InlineExpansion
Copyright   : (c) Roman Castellarin 2020.
License     : GPL-3
Stability   : experimental
-}

module InlineExpansion (optimize) where

import Lang
import Subst (subst, close)
import Debug.Trace (trace)
import Common (Pos(NoPos))
--import Debug.Trace (trace)

type Program = [Decl Ty Term]

pattern CONST :: Int -> Tm info ty var
pattern CONST n <- Const i (CNat n)

pattern LAM :: Tm info ty var -> Tm info ty var
pattern LAM t <- Lam i f ty t

-- TODO: is this correct? let e be locally nameless,
-- if x is free in e, e[t/x] == subst t (close x e)
expDecls :: Decl ty Term -> Program -> Program
expDecls def _ | trace ("expanding " ++ declName def ++ "\n") False = undefined 
expDecls def prog = fmap (subst (declBody def) . close (declName def)) <$> prog

-- criterion for expansion ??
-- probably based on size + nonrecursivity
shouldExpand :: Term -> Bool
shouldExpand term = case term of
    V {}        -> True
    Const {}    -> True
    -- truchisimo, solo para testear
    LAM (BinaryOp _ Plus _ Const {}) -> True
    _           -> False


expShortDefs :: Program -> Program
expShortDefs prog = foldr process prog prog
    where process d rest = if shouldExpand $ declBody d
                           then expDecls d rest else rest



-- TODO: when should I call recursively the optimization??
--       could this ever end up in an infinite loop?

solve :: Term -> Term
--solve term | trace ("solve " ++ show term ++ "\n") False = undefined 
solve term = case term of
    App _ (LAM t) c@CONST{}  -> subst c t
    App _ (LAM t) v@V{}      -> subst v t
    -- PCF is eager, is this worth optimizing?
    -- App i (Lam _ x xt t1) t2 -> Let i x xt t2 t1
    Let _ _ _ c@CONST{} t    -> subst c t
    Let _ _ _ v@V{}     t    -> subst v t
    t -> t

-- TODO: change function name
-- TODO: open / close names !!
cFold :: Term -> Term
cFold term = case term of
    Lam i x xt t            -> Lam i x xt (cFold t)
    App i t1 t2             -> solve $ App i (cFold t1) (cFold t2)
    BinaryOp i ty t1 t2     -> BinaryOp i ty (cFold t1) (cFold t2)
    Fix i f ft x xt t       -> Fix i f ft x xt (cFold t)
    Let i x xt t1 t2        -> solve $ Let i x xt (cFold t1) (cFold t2)
    IfZ i t1 t2 t3          -> IfZ i (cFold t1) (cFold t2) (cFold t3)
    t -> t

optimize :: Program -> Program
optimize = map (fmap cFold) . expShortDefs

-- DEBUGGING PURPOSES
x0 :: Term
x0 = App NoPos (Lam NoPos "x" NatTy 
                    (Lam NoPos "y" NatTy 
                        (BinaryOp NoPos Plus 
                            (BinaryOp NoPos Plus (V NoPos (Bound 1)) 
                            (V NoPos (Bound 1))) (V NoPos (Bound 0))))) 
                        (BinaryOp NoPos Plus (V NoPos (Free "const")) 
                    (Const NoPos (CNat 3)))