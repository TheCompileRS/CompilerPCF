{-|
Module      : Errors
Description : Definición del tipo de los errores
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Roman Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

-}

module Errors where

import Common (Pos)
import Text.Parsec.Error ( ParseError )

-- Agregar más, y source positions
data Error =
    ParseErr ParseError
  | ErrPos Pos String

instance Show Error where
  show (ParseErr e) = show e
  show (ErrPos p s) = show p++" "++ s
