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


-- | 'elab' elabora términos azucarados 
elab :: STerm -> Term
elab = elab_ . desugar

-- | 'elab_' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab_ :: NTerm -> Term
elab_ (V p v)               = V p (Free v)
elab_ (Const p c)           = Const p c
elab_ (Lam p v ty t)        = Lam p v ty (close v (elab_ t))
elab_ (App p h a)           = App p (elab_ h) (elab_ a)
elab_ (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab_ t))
elab_ (IfZ p c t e)         = IfZ p (elab_ c) (elab_ t) (elab_ e)
elab_ (UnaryOp i o t)       = UnaryOp i o (elab_ t)


elab_decl :: SDecl STerm -> Decl Term
elab_decl = fmap elab . desugar_decl

desugar_decl :: SDecl STerm -> Decl STerm
desugar_decl decl = case decl of
    SDecl p x ty t       -> Decl p x ty t
    SDeclLam p f ft bs t -> Decl p f (foldr chainTypes ft $ expandBinders bs) (SLam p bs t)
    SDeclFix p f ft bs t -> let ft' = foldr chainTypes ft $ expandBinders bs
                            in Decl p f ft' (SFix p f ft' bs t)

expandBinders :: MultiBinder -> [(Name, Ty)]
expandBinders bs = bs >>= \(vars, ty) -> flip (,) ty <$> vars 

chainTypes :: (Name, Ty) -> Ty -> Ty 
chainTypes (_, ty1) ty2 = FunTy ty1 ty2

constructFun :: info -> (Name, Ty) -> Tm info var -> Tm info var
constructFun i (var, ty) t = Lam i var ty t

-- | 'desugar' transforma términos azucarados en términos PCF0
desugar :: STerm -> NTerm
desugar term = case term of
    -- sugared
    SLet i x xt t1 t2           -> App i (Lam i x xt (desugar t2)) (desugar t1)

    SLetLam i f ft bs t1 t2     -> let bs' = expandBinders bs
                                   in desugar $ SLet i f (foldr chainTypes ft bs') (SLam i bs t1) t2

    SLetFix i f ft bs t1 t2     -> let ft' = (foldr chainTypes ft $ expandBinders bs)
                                   in desugar $ SLet i f ft' (SFix i f ft' bs t1) t2

    SLam i bs t                 -> foldr (constructFun i) (desugar t) (expandBinders bs)

    SFix i f ft bs t            -> let (x,xt):rest = expandBinders bs
                                   in Fix i f ft x xt $ foldr (constructFun i) (desugar t) rest
    -- base
    BV i x              -> V i x
    BConst i v          -> Const i v
    BApp i t1 t2        -> App i (desugar t1) (desugar t2)
    BUnaryOp i op t     -> UnaryOp i op (desugar t)
    
    BIfZ i t1 t2 t3     -> IfZ i (desugar t1) (desugar t2) (desugar t3)
      

