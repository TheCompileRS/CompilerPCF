module CanonConv where


import CIR
import ClosureConv
import Control.Monad.State (MonadState(get), modify, StateT)
import Control.Monad.Writer (Writer)
import Lang ( Const(CNat) )

-- ----------------------------------------------------

runCanon :: [IrDecl] -> CanonProg
runCanon decls = CanonProg (convertDecl <$> decls)  
 where convertDecl d = case d of
        IrVal{} -> Right $ irDeclName d
        IrFun{} -> undefined{-- Left ( irDeclName d,
                          irDeclArgNames d,
                          translate $ ?? )--} 

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
  _ -> undefined 
{--
IrIfZ t1 t2 t3 -> do
      loc_entry <- getLoc "entry"
      loc_then  <- getLoc "then"
      loc_else  <- getLoc "else"
      loc_cont  <- getLoc "cont"

      r_cond <- getNew
      v1 <- translate t1
      makeInst $ Assign r_cond $ V v1
      

      
      r2 <- getNew
      r3 <- getNew
      
      v2 <- translate t2
      v3 <- translate t3


      undefined

--}
