{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : InlineExpansion
Description : Optimizacion de InlineExpansion
Copyright   : (c) Roman Castellarin, Sebastián Zimmermann 2021.
License     : GPL-3
Stability   : experimental
-}

module InlineExpansion (optimize) where

import Lang
import Subst (subst, close, open, openN, closeN, substN)

type Program = [Decl Ty Term]

pattern CONST :: Int -> Tm info ty var
pattern CONST n <- Const i (CNat n)

pattern LAM :: Tm info ty var -> Tm info ty var
pattern LAM t <- Lam i f ty t

pattern FIX :: Tm info ty var -> Tm info ty var
pattern FIX t <- Fix i f ft x xt t

expDecls :: Decl ty Term -> Program -> Program
--expDecls def _ | trace ("expanding " ++ declName def ++ "\n") False = undefined 
expDecls def prog = fmap (subst (declBody def) . close (declName def)) <$> prog

size :: Term -> Int 
--size term | trace ("size " ++ show term ++ "\n") False = undefined 
size term = 1 + case term of
    Lam _ _ _ t        -> size t
    App _ t1 t2        -> size t1 + size t2
    BinaryOp _ _ t1 t2 -> size t1 + size t2
    Fix _ _ _ _ _ t    -> size t -- 11? hackazo
    Let _ _ _ t1 t2    -> size t1 + size t2
    IfZ _ t1 t2 t3     -> size t1 + size t2 + size t3
    t -> 0

expShortDefs :: Program -> Program
expShortDefs prog = foldr process prog prog
    where process d rest = if (<=15) . size . declBody $ d
                           then expDecls d rest else rest



-- TODO: when should I call recursively the optimization??
--       could this ever end up in an infinite loop?

solve :: Term -> Term
--solve term | trace ("solve " ++ show term ++ "\n") False = undefined 
solve term = case term of
    App _ (LAM t) c@CONST{}    -> subst c t
    App _ (LAM t) v@V{}        -> subst v t
    App _ l@(FIX t) c@CONST{}  -> substN [l, c] t
    App _ l@(FIX t) v@V{}      -> substN [l, v] t
    Let _ x _ c@CONST{} t      -> subst c t
    Let _ x _ v@V{}     t      -> subst v t
    -- PCF is eager, is this worth optimizing?
    -- App i (Lam _ x xt t1) t2 -> Let i x xt t2 t1
    t -> t

-- TODO: change function name
cFold :: Term -> Term
cFold term = case term of
    Lam i x xt t            -> Lam i x xt (close x $ cFold (open x t))
    App i t1 t2             -> solve $ App i (cFold t1) (cFold t2)
    BinaryOp i ty t1 t2     -> BinaryOp i ty (cFold t1) (cFold t2)
    Fix i f ft x xt t       -> Fix i f ft x xt (closeN [f, x] $ cFold $ openN [f, x] t)
    Let i x xt t1 t2        -> solve $ Let i x xt (cFold t1) (close x $ cFold (open x t2))
    IfZ i t1 t2 t3          -> IfZ i (cFold t1) (cFold t2) (cFold t3)
    t -> t

optimize :: Program -> Program
optimize = map (fmap cFold) . expShortDefs
