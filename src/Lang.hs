{-# LANGUAGE DeriveTraversable #-}


{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Román Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )
import Data.List (nub)

type Name = String

-- | AST de tipos
data Ty =
      NatTy
    | NatListTy
    | FunTy Ty Ty
    deriving (Show,Eq)

-- | AST de tipos azucarados
data STy =
      SNatTy
    | SNatListTy
    | SFunTy STy STy
    | SSynTy Name
    deriving (Show,Eq)

data Const = CNat Int
           | CLNat [Int]
  deriving Show

data UnaryOp = Succ | Pred | Head | Tail
  deriving Show

data BinaryOp = Plus
              | Minus
              | Times
              | Div
  deriving Show

type MultiBinder = [([Name], STy)]

-- | tipo de datos de declaraciones azucaradas, parametrizado por el tipo del cuerpo de la declaración
data SDecl a =
    SDecl     { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: STy, sDeclBody :: a }
  | SDeclLam  { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: STy, sDeclBinders :: MultiBinder, sDeclBody :: a }
  | SDeclFix  { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: STy, sDeclBinders :: MultiBinder, sDeclBody :: a }
  | SDeclType { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: STy }
  deriving (Show, Functor, Foldable, Traversable)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl ty a =
    Decl { declPos :: Pos, declName :: Name, declType :: ty, declBody :: a }
  deriving (Show,Functor)

-- | AST de los términos azucarados 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
-- los constructores que comienzan con S son azucarados y los que comienzan con B se corresponden con los de Tm
data STm info = SLet info Name STy (STm info) (STm info)                   -- ^ let x : s = t in t'
              | SLetLam info Name STy MultiBinder (STm info) (STm info) -- ^ let f (x1 : s1) ... (xn : sn) : s' = t in t'
              | SLetFix info Name STy MultiBinder (STm info) (STm info) -- ^ let rec f (x1 : s1) ... (xn : sn) : s' = t in t'
              | SLam info MultiBinder (STm info)                       -- ^ fun (x1 : s1) ... (xn : sn) -> t
              | SFix info Name STy MultiBinder (STm info)               -- ^ fix f (x1 : s1) ... (xn : sn) -> t
              | BV info Name
              | BConst info Const
              | BApp info (STm info) (STm info)
              | BUnaryOp info UnaryOp (STm info)
              | BBinaryOp info BinaryOp (STm info) (STm info)
              | BIfZ info (STm info) (STm info) (STm info)
    deriving (Show)

type STerm = STm Pos       -- ^ 'STm' tiene 'Name's como variables ligadas y libres, guarda posición. Admite azúcar sintáctico

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info ty var =
    V info var
  | Const info Const
  | Lam info Name ty (Tm info ty var)
  | App info (Tm info ty var) (Tm info ty var)
  | UnaryOp info UnaryOp (Tm info ty var)
  | BinaryOp info BinaryOp (Tm info ty var) (Tm info ty var)
  | Fix info Name ty Name ty (Tm info ty var)
  | IfZ info (Tm info ty var) (Tm info ty var) (Tm info ty var)
  | Let info Name ty (Tm info ty var) (Tm info ty var)
  deriving (Show, Functor)

type NTerm = Tm Pos Ty Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición
type Term = Tm Pos Ty Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición

data Var =
    Bound !Int
  | Free Name
  deriving Show

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info ty var -> info
getInfo (V i _) = i
getInfo (Const i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _ ) = i
--getInfo (UnaryOp i _ _) = i   -- UnaryOp inactive
getInfo (BinaryOp i _ _ _) = i
getInfo (Let i _ _ _ _ ) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars  :: Tm info ty Var -> [Name]
freeVars term = nub $ freeVars' term
  where
    freeVars'  (V _ (Free v))        = [v]
    freeVars'  (V _ _)               = []
    freeVars'  (Lam _ _ _ t)         = freeVars' t
    freeVars'  (App _ l r)           = freeVars' l ++ freeVars' r
    -- freeVars'  (UnaryOp _ _ t)       = freeVars' t   -- UnaryOp inactive
    freeVars'  (BinaryOp _ _ t1 t2)  = freeVars' t1 ++ freeVars' t2
    freeVars'  (Let _ _ _ t1 t2)     = freeVars' t1 ++ freeVars' t2
    freeVars'  (Fix _ _ _ _ _ t)     = freeVars' t
    freeVars'  (IfZ _ c t e)         = freeVars' c ++ freeVars' t ++ freeVars' e
    freeVars'  (Const _ _)           = []

binOpDef :: BinaryOp -> (Int -> Int -> Int)
binOpDef op = case op of
  Plus    -> (+)
  Minus   -> \ a b -> max 0 (a - b)
  Times   -> (*)
  Div     -> div

binOpSym :: BinaryOp -> String
binOpSym op = case op of
  Plus    -> "+"
  Minus   -> "-"
  Times   -> "*"
  Div     -> "/"