{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Román Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elabDecl ) where

import Lang
import Subst
import MonadPCF (failPCF, MonadPCF, lookupTyDef, addTyDef)



-- | 'elab' elabora términos azucarados, quitando el azucar del tipo, término y
-- transformándolo en un término con índices de de Bruijn 
elab :: MonadPCF m => STerm -> m Term
elab t1 = do
    t2 <- return $ desugarTerm t1
    t3 <- resolveTypesTerm t2
    t4 <- return $ bruijnize t3
    return t4


-- | 'elabDecl' elabora declaraciones, quitándoles el azucar sintáctico y resolviendo su tipo
elabDecl :: MonadPCF m => SDecl STerm -> m (Maybe (Decl Ty Term))
elabDecl decl = case decl of
    SDeclType _ name ty -> do ty' <- resolveType ty
                              addTyDef name ty'
                              return Nothing
    _ -> do decl2 <- return $ desugarDecl decl
            decl3 <- resolveTypesDecl decl2
            return $ Just decl3

-- | 'desugarDecl' quita el azucar sintáctico de una declaración.
desugarDecl :: SDecl STerm -> Decl STy STerm
desugarDecl decl = case decl of
     SDecl p x ty t       -> Decl p x ty t
     SDeclLam p f ft bs t -> Decl p f (foldr chainTypes ft $ expandBinders bs) (SLam p bs t)
     SDeclFix p f ft bs t -> let ft' = foldr chainTypes ft $ expandBinders bs
                             in Decl p f ft' (SFix p f ft' bs t)


-- | 'resolveTypesDecl' quita el azucar sintáctico de los tipos de una declaración.
resolveTypesDecl :: MonadPCF m => Decl STy STerm -> m (Decl Ty Term)
resolveTypesDecl decl = do ty <- resolveType $ declType decl
                           body <- elab $ declBody decl
                           return decl { declType = ty, declBody = body }


-- | 'desugarTerm' quita el azucar de los términos 
desugarTerm :: STm info -> Tm info STy Name
desugarTerm term = case term of
    -- sugared
    SLet i x xt t1 t2           -> Let i x xt (desugarTerm t1) (desugarTerm t2)
    SLetLam i f ft bs t1 t2     -> let bs' = expandBinders bs
                                   in desugarTerm $ SLet i f (foldr chainTypes ft bs') (SLam i bs t1) t2

    SLetFix i f ft bs t1 t2     -> let ft' = (foldr chainTypes ft $ expandBinders bs)
                                   in desugarTerm $ SLet i f ft' (SFix i f ft' bs t1) t2

    SLam i bs t                 -> foldr (constructFun i) (desugarTerm t) (expandBinders bs)

    SFix i f ft bs t            -> let (x,xt):rest = expandBinders bs
                                   in Fix i f ft x xt $ foldr (constructFun i) (desugarTerm t) rest
    -- base
    BV i x               -> V i x
    BConst i v           -> Const i v
    BApp i t1 t2         -> App i (desugarTerm t1) (desugarTerm t2)
    BUnaryOp i op t      -> desugarUnary i op (desugarTerm t)
    BBinaryOp i op t1 t2 -> BinaryOp i op (desugarTerm t1) (desugarTerm t2)
    BIfZ i t1 t2 t3      -> IfZ i (desugarTerm t1) (desugarTerm t2) (desugarTerm t3)
  where desugarUnary i op t = case op of
            Succ  -> BinaryOp i Plus  t (Const i (CNat 1))
            Pred  -> BinaryOp i Minus t (Const i (CNat 1))
            --op    -> UnaryOp i op t   -- UnaryOp inactive

-- | 'resolveTypesTerm' quita el azucar sintáctico de los términos
resolveTypesTerm :: MonadPCF m => Tm info STy var -> m (Tm info Ty var)
resolveTypesTerm term = case term of
    V i x       -> return $ V i x
    Const i v   -> return $ Const i v
    App i t1 t2 -> do   t1' <- resolveTypesTerm t1
                        t2' <- resolveTypesTerm t2
                        return $ App i t1' t2'
    -- UnaryOp inactive
    -- UnaryOp i op t -> do    t' <- resolveTypesTerm t
    --                         return $ UnaryOp i op t'
    BinaryOp i op t1 t2 -> do t1' <- resolveTypesTerm t1
                              t2' <- resolveTypesTerm t2    
                              return $ BinaryOp i op t1' t2'            
    IfZ i t1 t2 t3 -> do    t1' <- resolveTypesTerm t1
                            t2' <- resolveTypesTerm t2
                            t3' <- resolveTypesTerm t3
                            return $ IfZ i t1' t2' t3'
    Fix i f ft x xt t -> do t' <- resolveTypesTerm t
                            ft' <- resolveType ft
                            xt' <- resolveType xt
                            return $ Fix i f ft' x xt' t'
    Lam i x xt t -> do  t' <- resolveTypesTerm t
                        xt' <- resolveType xt
                        return $ Lam i x xt' t'
    Let i x xt t1 t2 -> do t1' <- resolveTypesTerm t1
                           t2' <- resolveTypesTerm t2
                           xt' <- resolveType xt
                           return $ Let i x xt' t1' t2'


expandBinders :: MultiBinder -> [(Name, STy)]
expandBinders bs = bs >>= \(vars, ty) -> flip (,) ty <$> vars 

chainTypes :: (Name, STy) -> STy -> STy 
chainTypes (_, ty1) ty2 = SFunTy ty1 ty2

constructFun :: info -> (Name, ty) -> Tm info ty var -> Tm info ty var
constructFun i (var, ty) t = Lam i var ty t

-- | 'resolveType' toma un tipo azucarado y le quita el azucar
resolveType :: MonadPCF m => STy -> m Ty
resolveType ty = case ty of
    SNatTy -> return NatTy
    SFunTy t1 t2 -> do t1' <- resolveType t1
                       t2' <- resolveType t2 
                       return $ FunTy t1' t2'
    SSynTy s -> do mt <- lookupTyDef s
                   case mt of
                       Just t  -> return t
                       Nothing -> failPCF $ "Tipo " ++ s ++ " desconocido."

-- | 'bruijnize' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
bruijnize :: NTerm -> Term
bruijnize (V p v)               = V p (Free v)
bruijnize (Const p c)           = Const p c
bruijnize (Lam p v ty t)        = Lam p v ty (close v (bruijnize t))
bruijnize (App p h a)           = App p (bruijnize h) (bruijnize a)
bruijnize (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (bruijnize t))
bruijnize (IfZ p c t e)         = IfZ p (bruijnize c) (bruijnize t) (bruijnize e)
-- bruijnize (UnaryOp i o t)       = UnaryOp i o (bruijnize t)      -- UnaryOp inactive
bruijnize (BinaryOp i o t1 t2)  = BinaryOp i o (bruijnize t1) (bruijnize t2)
bruijnize (Let p x xt t1 t2)    = Let p x xt (bruijnize t1) (close x (bruijnize t2))

     
