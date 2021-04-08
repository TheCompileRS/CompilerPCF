{-# LANGUAGE LambdaCase #-}
{-|
Module      : CanonConv
Description : Traduce a representación en bajo nivel.
Copyright   : (c) Román Castellarin, Sebastián Zimmermann, Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Stability   : experimental

Este modulo traduce declaraciones IrTerm a una representación en bajo nivel, que existe en el modulo CIR
-}

module CanonConv where

import CIR
import ClosureConv
import Control.Monad.State (MonadState(get), modify, StateT(runStateT))
import Control.Monad.Writer (runWriter, MonadWriter(tell), Writer)
import Lang ( Const(CNat) )
import Data.List (isPrefixOf)

-- ----------------------------------------------------


pcfMain :: [IrDecl] -> CanonM ()
pcfMain decls = do   openBlock "pcfmain"
                     val <- process decls
                     closeBlock $ Return val
    where process (d:ds) = do
                    t <- translate $ irDeclDef d
                    makeInst $ Store (irDeclName d) $ V t
                    if null ds then return t else process ds
          process [] = undefined


runCanon :: [IrDecl] -> CanonProg
runCanon decls = CanonProg $ canon_funs ++ canon_vals ++ [canon_main]
 where
        vals = filter (\case IrVal{} -> True; _ -> False) decls
        funs = filter (\case IrFun{} -> True; _ -> False) decls

        canon_vals = Right . irDeclName  <$> vals
        canon_funs = snd doFuns
        canon_main = Left ("pcfmain", [], canon_main_blocks )

        start_state = (0, "", [])
        canon_main_blocks = snd . runWriter $ runStateT (pcfMain vals) (fst doFuns)

        doFuns = foldr f (start_state, []) funs
            where f (IrFun name _ args body) (state, cfuns) =
                    let ((_, state'), b)  = runWriter $ runStateT funframe state
                    in (state', Left (name, args, b):cfuns)
                    where funframe = do
                            openBlock name
                            t <- translate body
                            closeBlock $ Return t

type CanonRes = Either CanonFun CanonVal

type CanonM = StateT (Int,Loc,[Inst]) (Writer Blocks)

getNew :: CanonM Reg
getNew = do (nreg, _, _) <- get
            modify $ \(n, l, s) -> (n+1, l, s)
            return $ Temp $ "reg_" ++ show nreg

getLoc :: String -> CanonM Loc
getLoc s = do (nloc, _, _) <- get
              modify $ \(n, l, w) -> (n+1, l, w)
              return $ s ++ show nloc

getCurrentLoc :: CanonM Loc
getCurrentLoc = do (_, l, _) <- get
                   return l

makeInst :: Inst -> CanonM ()
makeInst ins = modify $ \(n, l, s) -> (n, l, s++[ins])

openBlock :: Loc -> CanonM ()
openBlock l = modify $ \(n, _, s) -> (n, l, s)

closeBlock :: Terminator  -> CanonM ()
closeBlock t = do   (_, l, s) <- get
                    modify $ \(n, _, _) -> (n, "", [])
                    tell [(l, s, t)]

-- | 'translate' traduce terminos IrTerm a valores, guardando en CanonM (State)
-- los registros y blocks.
translate :: IrTerm -> CanonM Val
translate term = case term of
  IrVar x           -> do
      if "__" `isPrefixOf` x
          then return $ R (Temp x)
          else return $ G x
  IrConst (CNat x)  -> return $ C x
  IrBinaryOp op t1 t2 -> do
      r1 <- getNew
      r2 <- getNew
      r3 <- getNew
      v1 <- translate t1
      v2 <- translate t2
      makeInst $ Assign r1 $ V v1
      makeInst $ Assign r2 $ V v2
      makeInst $ Assign r3 $ BinOp op (R r1) (R r2)
      return $ R r3
  IrLet x t1 t2 -> do
      v1 <- translate t1
      makeInst $ Assign (Temp x) $ V v1
      translate t2
  IrCall f fs -> do
      r <- getNew
      vf <- translate f
      vfs <- mapM translate fs
      makeInst $ Assign r $ Call vf vfs
      return $ R r
  ClosureConv.MkClosure x ts -> do
      r <- getNew
      vs <- mapM translate ts
      makeInst $ Assign r $ CIR.MkClosure x vs
      return $ R r
  IrAccess t n -> do
      r <- getNew
      v <- translate t
      makeInst $ Assign r $ Access v n
      return $ R r
  IrIfZ t1 t2 t3 -> do
      loc_then  <- getLoc "then"
      loc_else  <- getLoc "else"
      loc_cont  <- getLoc "cont"


      v1 <- translate t1

      closeBlock $ CondJump (Eq v1 (C 0)) loc_then loc_else

      -- caso then
      openBlock loc_then
      r_then <- getNew
      v2 <- translate t2
      makeInst $ Assign r_then $ V v2
      loc_then_end <- getCurrentLoc
      closeBlock $ Jump loc_cont

      -- caso else
      openBlock loc_else
      r_else <- getNew
      v3 <- translate t3
      makeInst $ Assign r_else $ V v3
      loc_else_end <- getCurrentLoc
      closeBlock $ Jump loc_cont

      -- wrap up
      openBlock loc_cont
      r_cont <- getNew
      makeInst $ Assign r_cont $ Phi [(loc_then_end, R r_then), (loc_else_end, R r_else)]

      return $ R r_cont


