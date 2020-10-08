{-|
Module      : CEK
Description : ???
Copyright   : (c) Roman Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

Este módulo permite ??
-}

module CEK ( search, destroy,valToTerm ) where

import Lang
import MonadPCF
import Common
import Subst (substN)
--import Debug.Trace (trace)

data Val = VNat Int
         | CFun Env Name Ty Term 
         | CFix Env Name Ty Name Ty Term
     deriving Show

type Env = [Val]

data Fr = FApp Env Term
        | FClosure Val
        | FIfz Env Term Term
        | FUnaryOp UnaryOp
    deriving Show

type Kont = [Fr]

valToTerm :: Val -> Term
valToTerm v = case v of
    VNat n              -> Const NoPos (CNat n)
    CFun e x xt t       -> substN (valToTerm <$> e) $ Lam NoPos x xt t
    CFix e f ft x xt t  -> substN (valToTerm <$> e) $ Fix NoPos f ft x xt t

search :: MonadPCF m => Term -> Env -> Kont -> m Val
--search t e k | trace ("search " ++ show t ++ " <> " ++ show e ++ " <> " ++ show k) False = undefined
search t e k = case t of
        UnaryOp _ op t'  -> search t' e (FUnaryOp op : k)
        IfZ _ t1 t2 t3   -> search t1 e (FIfz e t2 t3 : k)
        App _ t1 t2      -> search t1 e (FApp e t2 : k)
        V i (Free x)     -> do mx <- lookupDecl x
                               case mx of
                                 Just y  -> search y e k
                                 Nothing -> failPosPCF i $ "Variable " ++ x ++ " no declarada." 
        -- TODO: does the type system guarantee that this won't go out of bounds?
        V _ (Bound n)    -> destroy (e !! n) k
        Const _ (CNat n) -> destroy (VNat n) k
        Lam _ x xt t      -> destroy (CFun e x xt t) k
        Fix _ f ft x xt t  -> destroy (CFix e f ft x xt t) k

destroy :: MonadPCF m => Val -> Kont -> m Val
--destroy v k | trace ("destroy " ++ show v ++ " <> " ++ show k) False = undefined
destroy v k = case k of
        []   -> return v
        fr:ks -> case fr of
            FUnaryOp Pred                 -> let VNat n = v
                                             in if n == 0 
                                                 then destroy (VNat 0) ks
                                                 else destroy (VNat $ n-1) ks
            FUnaryOp Succ                 -> let VNat n = v in destroy (VNat $ n+1) ks
            FIfz e t1 t2                  -> let VNat n = v
                                             in if n == 0
                                                 then search t1 e ks
                                                 else search t2 e ks
            FApp e t                      -> search t e (FClosure v : ks)
            FClosure (CFun e _ _ t)       -> search t (v : e) ks
            FClosure c@(CFix e _ _ _ _ t) -> search t (v : c : e) ks

