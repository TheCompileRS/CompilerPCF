{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Roman Castellarin, Sebastián Zimmermann 2020.
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

-- | AST de Tipos
data Ty = 
      NatTy 
    | FunTy Ty Ty
    deriving (Show,Eq)

type Name = String

data Const = CNat Int
  deriving Show

data UnaryOp = Succ | Pred
  deriving Show

type MultiBinder = [([Name], Ty)]

-- | tipo de datos de declaraciones azucaradas, parametrizado por el tipo del cuerpo de la declaración
data SDecl a =
    SDecl    { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: Ty, sDeclBody :: a }
  | SDeclLam { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: Ty, sDeclBinders :: MultiBinder, sDeclBody :: a }
  | SDeclFix { sDeclPos :: Pos, sDeclName :: Name, sDeclType :: Ty, sDeclBinders :: MultiBinder, sDeclBody :: a }
  deriving (Show,Functor)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a =
    Decl { declPos :: Pos, declName :: Name, declType :: Ty, declBody :: a }
  deriving (Show,Functor)

-- | AST de los términos azucarados 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
-- los constructores que comienzan con S son azucarados y los que comienzan con B se corresponden con los de Tm
data STm info = SLet info Name Ty (STm info) (STm info)                   -- ^ let x : s = t in t'
              | SLetLam info Name Ty MultiBinder (STm info) (STm info) -- ^ let f (x1 : s1) ... (xn : sn) : s' = t in t'
              | SLetFix info Name Ty MultiBinder (STm info) (STm info) -- ^ let rec f (x1 : s1) ... (xn : sn) : s' = t in t'
              | SLam info MultiBinder (STm info)                       -- ^ fun (x1 : s1) ... (xn : sn) -> t
              | SFix info Name Ty MultiBinder (STm info)               -- ^ fix f (x1 : s1) ... (xn : sn) -> t
              | BV info Name
              | BConst info Const
              | BApp info (STm info) (STm info)
              | BUnaryOp info UnaryOp (STm info)
              | BIfZ info (STm info) (STm info) (STm info)
    deriving (Show)

type STerm = STm Pos       -- ^ 'STm' tiene 'Name's como variables ligadas y libres, guarda posición. Admite azúcar sintáctico

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var = 
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | UnaryOp info UnaryOp (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  deriving (Show, Functor)

type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición
type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición

data Var = 
    Bound !Int
  | Free Name
  deriving Show

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _) = i
getInfo (Const i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _ ) = i
getInfo (UnaryOp i _ _) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))    = [v]
freeVars (V _ _)           = []
freeVars (Lam _ _ _ t)     = freeVars t
freeVars (App _ l r)       = freeVars l ++ freeVars r
freeVars (UnaryOp _ _ t)   = freeVars t
freeVars (Fix _ _ _ _ _ t) = freeVars t
freeVars (IfZ _ c t e)     = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)       = []
