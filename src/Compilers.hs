{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Compilers
Description : Scripts de compilacion de PCF.
Copyright   : (c) Román Castellarin, Sebastián Zimmermann, Mauro Jaskelioff, Guido Martínez, 2021.
License     : GPL-3
Stability   : experimental

-}


module Compilers where


import MonadPCF
import Parse (P, runP, program)
import Errors
import Control.Exception (IOException, catch)
import Lang
import Global (GlEnv(..))
import Data.Char (isSpace)
import System.IO ( stderr, hPutStr )
import Elab (elabDecl)
import TypeChecker (tcDecl)
import qualified DeadCode (optimize)
import qualified ConstantFolding (optimize)
import qualified InlineExpansion (optimize)
import qualified Data.Text.Lazy.IO as TIO
--import Data.Text.Lazy (putStrLn)
import Data.Maybe (catMaybes)
import Data.List (intercalate, dropWhileEnd)
import CanonConv (runCanon)
import ClosureConv (runCC)
import InstSel (codegen)
import LLVM.Pretty (ppllvm)
import System.Process (system)
import Bytecompile (bcRead, runBC, bytecompileModule, bcWrite)

data CompilationOptions = CompilationOptions {
      showBase :: Bool,
      showDesugar :: Bool,
      showOptimized :: Bool,
      showClosures :: Bool,
      showCanonized :: Bool,
      showLLVM :: Bool,
      nOpt :: Int
}

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r  

trim :: String -> String 
trim = dropWhileEnd isSpace . dropWhile isSpace

openFilePCF :: MonadPCF m => String -> m String
openFilePCF filename = do
    printPCF $ "Abriendo " ++ filename ++ "..."
    liftIO $ catch (readFile filename)
             (\e -> do let err = show (e :: IOException)
                       hPutStr stderr $ "No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++ "\n"
                       return "")


compileFiles :: MonadPCF m => [String] -> m ()
compileFiles = mapM_ $ \filename -> do
        modify (\s -> s { lfile = filename, inter = False })
        compileFile filename

compileFile ::  MonadPCF m => String -> m ()
compileFile filename = do
    x <- openFilePCF filename
    decls <- parseIO filename program x
    mapM_ handleDecl decls

handleDecl ::  MonadPCF m => SDecl STerm -> m ()
handleDecl decl = do
  decl' <- elabDecl decl
  case decl' of
    Nothing -> return ()
    Just (Decl p x ty tt) -> do
        tcDecl (Decl p x ty tt)
        addDecl (Decl p x ty tt)

----------------------------------------
--          INDEPENDENT CODE          --
----------------------------------------

-- GENERAL COMPILATION

compileClosures :: MonadPCF m => [String] -> m ()
compileClosures []     = return ()
compileClosures (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileClosure x
        compileClosures xs

compileClosure :: MonadPCF m => String -> m ()
compileClosure file = do
    let filename = trim file
    x <- openFilePCF filename
    sdecls <- parseIO filename program x
    catchErrors $ mapM_ handleDecl sdecls -- type checking
    decls' <- catMaybes <$> mapM elabDecl sdecls
    printPCF $ ("\nORIGINALLY\n" ++) $ intercalate "\n" $ show <$> decls'
    -- HACEMOS N=20 RONDAS AHRE
    let optimizer = InlineExpansion.optimize . DeadCode.optimize . ConstantFolding.optimize
    let decls = iterate optimizer decls' !! 20
    printPCF $ ("\nOPTIMIZED\n" ++) $ intercalate "\n" $ show <$> decls
    printPCF $ ("\nCLOSURE CONVERSIONS\n" ++) $ intercalate "\n" $ show <$> runCC decls
    let canonprog = runCanon. runCC $ decls
    printPCF $ "\nCANONIZED PROGRAM\n" ++ show canonprog
    let irdecls = codegen canonprog
    liftIO $ TIO.writeFile "output.ll" (ppllvm irdecls)
    let commandline = "clang -Wno-override-module output.ll src/runtime.c -lgc -o prog"
    liftIO $ system commandline
    return ()

-- --------------------------------------------

compilation :: MonadPCF m => CompilationOptions -> [String] -> m ()
compilation opts files = do
    compileFiles $ trim <$> files

    -- read file
    x <- openFilePCF filename
    ds_base <- parseIO filename program x
    -- show base
    when (showBase opts) $
        -- TODO should we print this or pretty print?
        printPCF $ ("BASE CODE\n" ++) $ intercalate "\n" $ show <$> ds_base
    -- type checking
    catchErrors $ mapM_ handleDecl ds_base 
    -- elaboration
    ds_elab <- catMaybes <$> mapM elabDecl ds_base
    -- show desugar
    when (showDesugar opts) $
        printPCF $ ("DESUGARED CODE\n" ++) $ intercalate "\n" $ show <$> ds_elab
    -- optimization
    let optimizer = InlineExpansion.optimize . DeadCode.optimize . ConstantFolding.optimize
    let ds_opt = iterate optimizer ds_elab !! nOpt opts
    -- show optimized
    when (showOptimized opts) $
        printPCF $ ("OPTIMIZED CODE\n" ++) $ intercalate "\n" $ show <$> ds_opt
    -- closure conversion
    let ds_closure = runCC ds_opt
    -- show closures
    when (showClosures opts) $
        printPCF $ ("CLOSURE CONVERSIONS\n" ++) $ intercalate "\n" $ show <$> ds_closure
    -- canonization
    let ds_canon = runCanon ds_closure
    -- show closures
    when (showCanonized opts) $
        printPCF $ "CANONIZED CODE\n" ++ show ds_canon
    -- intermediate representation
    let ds_ir = codegen ds_canon
    -- show IR
        -- <TODO>
    -- LLVM translation
    let ds_llvm = ppllvm ds_ir
    liftIO $ TIO.writeFile "output.ll" ds_llvm
    -- show LLVM
    when (showLLVM opts) $
        printPCF $ "LLVM PROGRAM\n" ++ (read . show) ds_llvm
    -- LLVM compilation
    let commandline = "clang -Wno-override-module output.ll src/runtime.c -lgc -o prog"
    liftIO $ system commandline
    return ()

-- BYTECODE COMPILATION

bcRunFile :: MonadPCF m => String -> m ()
bcRunFile filename = do
        file <- liftIO $ bcRead filename
        runBC file


bcCompileFiles :: MonadPCF m => [String] -> m ()
bcCompileFiles = mapM_ $ \filename -> do
        modify (\s -> s { lfile = filename, inter = False })
        bcCompileFile filename

bcCompileFile ::  MonadPCF m => String -> m ()
bcCompileFile f = do
    let filename = trim f
    x <- openFilePCF filename
    sdecls <- parseIO filename program x
    decls <- catMaybes <$> mapM elabDecl sdecls
    code <- bytecompileModule decls
    let newFilename = takeWhile (/='.') filename ++ ".byte"
    printPCF $ show $ length code
    liftIO $ bcWrite code newFilename