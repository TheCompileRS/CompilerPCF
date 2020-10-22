{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Roman Castellarin, Sebastián Zimmermann, Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, bytecompileModule, runBC, bcWrite, bcRead)
 where

import Lang 
import Subst
import MonadPCF

-- for debugging
import Parse
import Elab

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejecutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern SUCC     = 6
pattern PRED     = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14

bc :: MonadPCF m => Term -> m Bytecode
bc term = case term of
    V _ (Bound i)     -> return [ACCESS, i]
    V i (Free x)      -> do mx <- lookupDecl x
                            case mx of
                              Just y  -> bc y
                              Nothing -> failPosPCF i $ "Variable " ++ x ++ " no declarada." 
    Const _ (CNat n)  -> return [CONST, n]
    Lam _ _ _ t       -> do p <- bc t
                            return $ [FUNCTION, length p + 1] ++ p ++ [RETURN]
    App _ t1 t2       -> do p1 <- bc t1
                            p2 <- bc t2
                            return $ p1 ++ p2 ++ [CALL]
    UnaryOp _ op t    -> do p <- bc t
                            return $ p ++ [compileUnaryOp op]
    IfZ _ t1 t2 t3    -> do p1 <- bc t1
                            p2 <- bc t2
                            p3 <- bc t3
                            return $ p3 ++ p2 ++ p1 ++ [IFZ]
    Fix _ _ _ _ _ t   -> do p <- bc t
                            return $ [FUNCTION, length p + 1] ++ p ++ [RETURN, FIX]
  where 
    compileUnaryOp op = case op of
      Succ -> SUCC
      Pred -> PRED

type Module = [Decl Ty Name] -- ?? que es Module??
bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule _ = error "implementame"

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadPCF m => Bytecode -> m ()
runBC prog = case prog of 
  
  _ -> error "implementame"


---------------------------
-- * SANDBOX
---------------------------

parseTerm :: String -> STerm
parseTerm s = case runP tm s "parseTerm" of
      Right a -> a 

mt :: MonadPCF m => m Bytecode
mt = bc =<< (elab . parseTerm) "fix (f: Nat -> Nat) (n: Nat) -> ifz n then 0 else f (pred n)"

