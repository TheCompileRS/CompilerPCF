{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Román Castellarin, Sebastián Zimmermann 2020-2021.
License     : GPL-3
Stability   : experimental

-}
module Global where

import Lang

data GlEnv = GlEnv {
  inter :: Bool,         -- ^ True, si estamos en modo interactivo.
  lfile :: String,       -- ^ Último archivo cargado.
  glb :: [Decl Ty Term], -- ^ Entorno con declaraciones globales
  tyEnv :: [(Name,Ty)],  -- ^ Entorno de tipado de declaraciones globales
  tyDefs :: [(Name,Ty)]  -- ^ Sinónimos de tipo
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" [] [] []
