{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : ConstantFolding
Description : Optimización de constant folding
Copyright   : (c) Roman Castellarin, Sebastián Zimmermann 2021.
License     : GPL-3
Stability   : experimental
-}

module ConstantFolding (optimize) where

import Lang

pattern CONST :: Int -> Tm info ty var
pattern CONST n <- Const i (CNat n)

solve :: Term -> Term
solve term = case term of
    BinaryOp i Plus     t1          (CONST 0)   -> t1
    BinaryOp i Plus     (CONST 0)   t2          -> t2
    BinaryOp i Minus    t1          (CONST 0)   -> t1
    BinaryOp i Minus    (CONST 0)   t2          -> Const i (CNat 0)
    BinaryOp i Plus     (CONST a)   (CONST b)   -> Const i (CNat $ binOpDef Plus a b)
    BinaryOp i Minus    (CONST a)   (CONST b)   -> Const i (CNat $ binOpDef Minus a b)
    BinaryOp i Times    t1          (CONST 0)   -> Const i (CNat 0)
    BinaryOp i Times    (CONST 0)   t2          -> Const i (CNat 0)
    BinaryOp i Times    t1          (CONST 1)   -> t1
    BinaryOp i Times    (CONST 1)   t2          -> t2
    BinaryOp i Times    (CONST a)   (CONST b)   -> Const i (CNat $ binOpDef Times a b)
    BinaryOp i Div      t1          (CONST 1)   -> t1
    BinaryOp i Div      (CONST a)   (CONST b)   
                                       | b /= 0 -> Const i (CNat $ binOpDef Div a b)
    t -> t

cFold :: Term -> Term 
cFold term = case term of
    Lam i f ty t            -> Lam i f ty (cFold t)
    App i t1 t2             -> App i (cFold t1) (cFold t2)
    BinaryOp i ty t1 t2     -> solve $ BinaryOp i ty (cFold t1) (cFold t2)
    Fix i f ft x xt t       -> Fix i f ft x xt (cFold t)
    Let i x xt t1 t2        -> Let i x xt (cFold t1) (cFold t2)
    IfZ i t1 t2 t3          -> case cFold t1 of 
                                    CONST 0 -> cFold t2
                                    CONST _ -> cFold t3
                                    c       -> IfZ i c (cFold t2) (cFold t3)
    t -> t

optimize :: [Decl Ty Term] -> [Decl Ty Term]
optimize = map $ fmap cFold 

