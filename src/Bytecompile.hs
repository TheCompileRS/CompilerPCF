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
import Debug.Trace (trace)

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
   -- _ | trace ("bcing " ++ show term ++ "\n") False -> undefined
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
                            let l2 = length p2
                            let l3 = length p3
                            return $ p1 ++ [IFZ, l2 + 2] ++ p2 ++ [JUMP, l3] ++ p3
    Fix _ _ _ _ _ t   -> do p <- bc t
                            return $ [FUNCTION, length p + 1] ++ p ++ [RETURN, FIX]
    Let _ _ _ t1 t2   -> do p1 <- bc t1
                            p2 <- bc t2
                            return $ p1 ++ [SHIFT] ++ p2 ++ [DROP]
  where 
    compileUnaryOp op = case op of
      Succ -> SUCC
      Pred -> PRED

bytecompileModule :: MonadPCF m => [Decl Ty Term] -> m Bytecode
bytecompileModule prog = do
    let orto = foldr letter lastSymbol prog
    --printPCF $ show orto
    code <- bc $ orto 
    printPCF $ show code
    return $ code ++ [PRINT, STOP]
 where 
  lastDecl = last prog
  lastSymbol = V (declPos lastDecl) $ Free $ declName lastDecl
  letter d term = Let (declPos d) (declName d) (declType d) (declBody d) (close (declName d) term)

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
runBC prog = bVM (prog, [], [])

data Val = I Int | Fun Env Bytecode | RA Env Bytecode
  deriving Show
type Env = [Val]
type Stack = [Val]

bVM :: MonadPCF m => (Bytecode, Env, Stack) -> m ()
bVM stateVM = case stateVM of
  (RETURN:_        , _        , val:RA env prog:stack) -> bVM (prog, env, val:stack) 
  (CONST:val:prog  , env      , stack                   ) -> bVM (prog         , env       , I val:stack           )
  (ACCESS:i:prog   , env      , stack                   ) -> bVM (prog         , env       , env!!i:stack          )
  (FUNCTION:l:rest , env      , stack                   ) -> bVM (prog'        , env       , Fun env body:stack    )
                                                       where (body , prog') = splitAt l rest
  (CALL:prog       , env      , val:Fun efun body:stack ) -> bVM (body         , val:efun  , RA env prog : stack   )
  (SUCC:prog       , env      , I n:stack               ) -> bVM (prog         , env       , I (n+1):stack         )
  (PRED:prog       , env      , I n:stack               ) -> bVM (prog         , env       , I (max 0 (n-1)):stack )
  (FIX:prog        , env      , Fun efun body:stack     ) -> bVM (prog         , env       , Fun efix body:stack   )
                                                       where efix = Fun efix body : efun
  (IFZ:l:prog      , env      , I n:stack               ) -> if n == 0 
                                                        then bVM (prog         , env       , stack                 )
                                                        else bVM (drop l prog  , env       , stack                 )
  (JUMP:l:prog     , env      , stack                   ) -> bVM (drop l prog  , env       , stack                 )
  (SHIFT:prog      , env      , val:stack               ) -> bVM (prog         , val:env   , stack                 )
  (DROP:prog       , val:env  , stack                   ) -> bVM (prog         , env       , stack                 )
  (STOP:_          , _        , _                       ) -> return ()
  (PRINT:_         , _        , val:_                   ) -> liftIO $ print val
  _ -> liftIO $ putStrLn $ "Unrecognized bytecode/state" ++ show stateVM
    


---------------------------
-- * SANDBOX
---------------------------

parseTerm :: String -> STerm
parseTerm s = case runP tm s "parseTerm" of
      Right a -> a 

mt :: MonadPCF m => m Bytecode
--mt = bc =<< (elab . parseTerm) "fix (f: Nat -> Nat) (n: Nat) -> ifz n then 0 else f (pred n)"
mt = bc =<< (elab . parseTerm) "(fix (suma: Nat -> Nat -> Nat)  (x y: Nat) -> ifz y then x else succ (suma x (pred y))) 100000 23456"

mtc :: MonadPCF m => m ()
mtc = mt >>= runBC