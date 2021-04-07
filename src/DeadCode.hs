{-|
Module      : DeadCode
Description : Optimizacion de dead code
Copyright   : (c) Román Castellarin, Sebastián Zimmermann 2021.
License     : GPL-3
Stability   : experimental
-}

module DeadCode (optimize) where

import Lang
import Data.List (find)
import Control.Monad (ap)

type Program = [Decl Ty Term] 

findDecls :: Program -> Decl Ty Term -> [Name]
findDecls prog decl = declName decl : ((freeVars . declBody) decl >>= go)
    where go name = case find ((name ==) . declName) prog of
            Just newDecl -> findDecls prog newDecl
            Nothing      -> []

cleanDecls :: Program -> [Name] -> Program
cleanDecls prog keepList = filter ((`elem` keepList) . declName) prog

optimize :: [Decl Ty Term] -> [Decl Ty Term]
optimize = ap cleanDecls (ap findDecls last)

