{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Roman Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl ) where

import Lang
import Subst

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: NTerm -> Term
elab (V p v)               = V p (Free v)
elab (Const p c)           = Const p c
elab (Lam p v ty t)        = Lam p v ty (close v (elab t))
elab (App p h a)           = App p (elab h) (elab a)
elab (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab t))
elab (IfZ p c t e)         = IfZ p (elab c) (elab t) (elab e)
elab (UnaryOp i o t)       = UnaryOp i o (elab t)

elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab

-- | 'desugar' transforma términos azucarados en términos PCF0
desugar :: STerm -> NTerm
desugar term = case term of
    -- sugared
    SLet i x xt t1 t2       -> undefined
    SLetLam i f ft bs t1 t2 -> undefined
    SLetFix i f ft bs t1 t2 -> undefined
    SLam i bs t             -> undefined
    -- base
    BV i x              -> V i x
    BConst i v          -> Const i v
    BApp i t1 t2        -> App i (desugar t1) (desugar t2)
    BUnaryOp i op t     -> UnaryOp i op (desugar t)
    BFix i f ft bs t    -> undefined
    BIfZ i t1 t2 t3     -> IfZ i (desugar t1) (desugar t2) (desugar t3)
