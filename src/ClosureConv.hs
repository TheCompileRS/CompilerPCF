{-|
Module      : ClosureConv
Description : Implementación de una conversión de clausuras y hoisting
Copyright   : (c) Román Castellarin, Sebastián Zimmermann, 2020.
License     : GPL-3
Stability   : experimental
-}
module ClosureConv
  (runCC, IrTerm(..), IrDecl(..))
 where

import Lang 
import MonadPCF

import Control.Monad.Writer (runWriter, tell, Writer)
import Subst (openN, open)
import Data.List (isPrefixOf)

-- | Términos intermedios
data IrTerm = IrVar Name
            | IrCall IrTerm [IrTerm]
            | IrConst Const
            | IrBinaryOp BinaryOp IrTerm IrTerm
            | IrLet Name IrTerm IrTerm
            | IrIfZ IrTerm IrTerm IrTerm
            | MkClosure Name [IrTerm]
            | IrAccess IrTerm Int
      deriving Show

-- | Declaraciones globales
data IrDecl = IrVal { irDeclName :: Name, irDeclDef :: IrTerm }
            | IrFun { irDeclName :: Name, irDeclArity :: Int, irDeclArgNames :: [Name], irDeclBody :: IrTerm }
    deriving Show

-- | 'ConvertMonad' es una mónada cuyo estado reprensenta un contador de nombres frescos,
-- mientras que el Writer se encarga de recolectar las definiciones globales.
type ConvertMonad = StateT Int (Writer [IrDecl])


makeTerm :: Name -> [Name] -> IrTerm -> IrTerm
makeTerm cloName fv t = foldr f t (zip fv [1..])
  where f (v, i) b = IrLet v (IrAccess (IrVar cloName) i) b  

makeTermR :: Name -> [Name] -> IrTerm -> IrTerm
makeTermR cloName (f:fv) t = IrLet f (IrVar cloName) (makeTerm cloName fv t)
makeTermR _ [] _ = error "second argument must not be empty"

genName :: String -> ConvertMonad String
genName s = do n <- get
               modify (+1)
               return $ "__" ++ s ++ show n 

-- | 'closureConvert' transforma los términos en clasuras.
closureConvert :: Term -> ConvertMonad IrTerm
closureConvert term = case term of
    V _ (Free var)      -> return $ IrVar var
    V _ (Bound _)       -> error "variable should have been opened by this point"
    Const _ c           -> return $ IrConst c  
    Lam _ x _ t         -> do funName <- genName ""
                              varName <- genName x
                              cloName <- genName "clo"
                              t' <- closureConvert (open varName t)
                              let fv = filter ("__" `isPrefixOf`) $ freeVars t
                              let lets = makeTerm cloName fv t' 
                              tell [IrFun funName 2 [cloName, varName] lets]
                              return $ MkClosure funName (IrVar <$> fv)
    App _ f x           -> do f' <- closureConvert f
                              x' <- closureConvert x
                              return $ IrCall (IrAccess f' 0) [f', x']
    UnaryOp {}          -> error "unary operators should have been translated to binary"
    BinaryOp _ op t1 t2 -> do t1' <- closureConvert t1
                              t2' <- closureConvert t2
                              return $ IrBinaryOp op t1' t2'
    Fix _ f _ x _ t     -> do funName <- genName ""
                              varName <- genName x
                              cloName <- genName "clo"
                              lamName <- genName f
                              t' <- closureConvert (openN [lamName, varName] t)
                              let fv = filter ("__" `isPrefixOf`) $ freeVars t
                              let lets = makeTermR cloName (lamName:fv) t' 
                              tell [IrFun funName 2 [cloName, varName] lets]
                              return $ MkClosure funName (IrVar <$> fv)
    IfZ _ t1 t2 t3      -> do t1' <- closureConvert t1
                              t2' <- closureConvert t2
                              t3' <- closureConvert t3
                              return $ IrIfZ t1' t2' t3'                   
    Let _ v _ t1 t2     -> do t1' <- closureConvert t1
                              varName <- genName v
                              t2' <- closureConvert (open varName t2)
                              return $ IrLet varName t1' t2'

runCC :: [Decl ty Term] -> [IrDecl]
runCC xs = snd $ runWriter $ runStateT (mapM_ f xs) 0
  where f decl = do t <- closureConvert (declBody decl)
                    tell [IrVal (declName decl) t]

