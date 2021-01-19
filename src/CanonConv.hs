{-# LANGUAGE LambdaCase #-}

module CanonConv where


import CIR
import ClosureConv
import Control.Monad.State (MonadState(get), modify, StateT(runStateT))
import Control.Monad.Writer (runWriter, MonadWriter(tell), Writer)
import Lang ( Const(CNat) )

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


{-
runCanon :: [IrDecl] -> CanonProg
runCanon decls = CanonProg $ canon_funs ++ canon_vals ++ [canon_main]
 where 
        vals = filter (\case IrVal{} -> True; _ -> False) decls
        funs = filter (\case IrFun{} -> True; _ -> False) decls
        
        canon_vals = Right . irDeclName  <$> vals
        canon_funs = convertFuns <$> funs
        canon_main = Left ("pcfmain", [], canon_main_blocks )

        start_state = (0, "", [])
        canon_main_blocks = snd . runWriter $ runStateT (pcfMain vals) start_state
        convertFuns d = Left (irDeclName d, irDeclArgNames d, blocks)
         where blocks = snd . runWriter $ runStateT (translate $ irDeclBody d) start_state
-}

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

-- data IrTerm = IrVar Name
--             | IrCall IrTerm [IrTerm]
--             | IrConst Const
--             | IrBinaryOp BinaryOp IrTerm IrTerm
--             | IrLet Name IrTerm IrTerm
--             | IrIfZ IrTerm IrTerm IrTerm
--             | MkClosure Name [IrTerm]
--             | IrAccess IrTerm Int
--       deriving Show

getNew :: CanonM Reg
getNew = do (nreg, _, _) <- get
            modify $ \(n, l, s) -> (n+1, l, s)
            return $ Temp $ "reg_" ++ show nreg

getLoc :: String -> CanonM Loc
getLoc s = do (nloc, _, _) <- get
              modify $ \(n, l, w) -> (n+1, l, w)
              return $ s ++ show nloc

makeInst :: Inst -> CanonM ()
makeInst ins = modify $ \(n, l, s) -> (n, l, s++[ins])

openBlock :: Loc -> CanonM ()
openBlock l = modify $ \(n, _, s) -> (n, l, s)

closeBlock :: Terminator  -> CanonM ()
closeBlock t = do   (_, l, s) <- get
                    modify $ \(n, _, _) -> (n, "", [])
                    tell [(l, s, t)]

translate :: IrTerm -> CanonM Val
translate term = case term of
  IrVar x           -> return $ G x 
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
      r <- getNew
      v1 <- translate t1
      makeInst $ Store x $ V v1
      v2 <- translate t2
      makeInst $ Assign r $ V v2
      return $ R r
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
      --loc_entry <- getLoc "entry"
      loc_then  <- getLoc "then"
      loc_else  <- getLoc "else"
      loc_cont  <- getLoc "cont"

      
      --r_cond <- getNew
      v1 <- translate t1
      --makeInst $ Assign r_cond $ V v1
      
      closeBlock $ CondJump (Eq v1 (C 0)) loc_then loc_else

      -- caso then
      openBlock loc_then
      r_then <- getNew
      v2 <- translate t2
      makeInst $ Assign r_then $ V v2
      closeBlock $ Jump loc_cont

      -- caso else
      openBlock loc_else
      r_else <- getNew
      v3 <- translate t3
      makeInst $ Assign r_else $ V v3
      closeBlock $ Jump loc_cont

      -- wrap up
      openBlock loc_cont
      r_cont <- getNew
      makeInst $ Assign r_cont $ Phi [(loc_then, R r_then), (loc_else, R r_else)]

      return $ R r_cont


