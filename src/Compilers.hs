

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
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import CanonConv (runCanon)
import ClosureConv (runCC)
import InstSel (codegen)
import LLVM.Pretty (ppllvm)
import System.Process (system)
import Bytecompile (bcRead, runBC, bytecompileModule, bcWrite)

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r  

compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

compileFile ::  MonadPCF m => String -> m ()
compileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
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
    printPCF ("Abriendo "++file++"...")
    let filename = reverse(dropWhile isSpace (reverse file))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
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


-- BYTECODE COMPILATION

bcRunFile :: MonadPCF m => String -> m ()
bcRunFile filename = do
        file <- liftIO $ bcRead filename
        runBC file

bcCompileFiles ::  MonadPCF m => [String] -> m ()
bcCompileFiles []     = return ()
bcCompileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        bcCompileFile x
        bcCompileFiles xs

bcCompileFile ::  MonadPCF m => String -> m ()
bcCompileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    sdecls <- parseIO filename program x
    decls <- catMaybes <$> mapM elabDecl sdecls
    code <- bytecompileModule decls
    let newFilename = take (length filename - 3) filename ++ "byte"
    printPCF $ show $ length code
    liftIO $ bcWrite code newFilename