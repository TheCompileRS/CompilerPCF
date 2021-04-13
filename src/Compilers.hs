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
import Data.List (intercalate, dropWhileEnd)
import CanonConv (runCanon)
import ClosureConv (runCC)
import InstSel (codegen)
import LLVM.Pretty (ppllvm)
import System.Process (system)
import Bytecompile (bcRead, runBC, bytecompileModule, bcWrite)

-- | Compilation Options
data CompilationOptions = CompilationOptions {
      showBase :: Bool,
      showDesugar :: Bool,
      showOptimized :: Bool,
      showClosures :: Bool,
      showCanonized :: Bool,
      showIR :: Bool,
      showLLVM :: Bool,
      nOpt :: Int
}

-- | Run a parser, raising exceptions in case of error
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r  

-- | Remove whitespace from string ends
trim :: String -> String 
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Read contents of file
-- nothrow: in case of error prints message and returns empty string
openFilePCF :: MonadPCF m => String -> m String
openFilePCF filename = do
    printPCF $ "Abriendo " ++ filename ++ "..."
    liftIO $ catch (readFile filename)
             (\e -> do let err = show (e :: IOException)
                       hPutStr stderr $ "No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++ "\n"
                       return "")

-- | Load list of files into memory
loadFiles :: MonadPCF m => Bool -> [String] -> m ()
loadFiles showCode = mapM_ $ \filename -> do
        modify (\s -> s { lfile = filename, inter = False })
        loadFile showCode filename

-- | Load file into memory
loadFile ::  MonadPCF m => Bool -> String -> m ()
loadFile showCode filename = do
    -- read contents
    contents <- openFilePCF filename
    decls <- parseIO filename program contents
    -- show base code
    when showCode $ do
        -- TODO should we print this or pretty print?
        printPCF $ "\nBASE CODE " ++ filename ++ "\n" 
        printPCF $ intercalate "\n" $ show <$> decls
    -- typechecking and elaboration
    mapM_ handleDecl decls

-- | Process declaration and keep it in memory 
handleDecl ::  MonadPCF m => SDecl STerm -> m ()
handleDecl decl = do
  d_elab <- elabDecl decl -- elaboration
  forM_ d_elab $ \d -> do
      tcDecl d   -- typechecking
      addDecl d  -- storing

typechecking :: MonadPCF m => [String] -> m ()
typechecking files = do 
    loadFiles False files
    decls <- glb <$> get
    when (null decls) $
        failPCF "Sin declaraciones."
    printPCF "El programa tipa correctamente."

-- | General compilation
compilation :: MonadPCF m => CompilationOptions -> [String] -> m ()
compilation opts files = do
    -- read files
    loadFiles (showBase opts) $ trim <$> files
    -- retreive processed declarations
    ds_elab <- reverse . glb <$> get
    when (null ds_elab) $
        failPCF "Sin declaraciones."
    -- show desugared code
    when (showDesugar opts) $
        printPCF $ ("\nDESUGARED CODE\n" ++) $ intercalate "\n" $ show <$> ds_elab
    -- optimization
    let optimizer = InlineExpansion.optimize 
                  . DeadCode.optimize 
                  . ConstantFolding.optimize
    let ds_opt = iterate optimizer ds_elab !! nOpt opts
    -- show optimized code
    when (showOptimized opts) $
        printPCF $ ("\nOPTIMIZED CODE\n" ++) $ intercalate "\n" $ show <$> ds_opt
    -- closure conversion
    let ds_closure = runCC ds_opt
    -- show closures
    when (showClosures opts) $
        printPCF $ ("\nCLOSURE CONVERSIONS\n" ++) $ intercalate "\n" $ show <$> ds_closure
    -- canonization
    let ds_canon = runCanon ds_closure
    -- show closures
    when (showCanonized opts) $
        printPCF $ "\nCANONIZED CODE\n" ++ show ds_canon
    -- intermediate representation
    let ds_ir = codegen ds_canon
    -- show IR
    when (showIR opts) $
        printPCF $ "\nINTERMEDIATE REPRESENTATION\n" ++ show ds_ir
    -- LLVM translation
    let ds_llvm = ppllvm ds_ir
    liftIO $ TIO.writeFile "output.ll" ds_llvm
    -- show LLVM
    when (showLLVM opts) $
        printPCF $ "\nLLVM PROGRAM\n" ++ (read . show) ds_llvm
    -- LLVM compilation
    let commandline = "clang -Wno-override-module output.ll src/runtime.c -lgc -o prog"
    liftIO $ system commandline
    printPCF "Compilación exitosa!!"
 
-- BYTECODE COMPILATION

bcRunFile :: MonadPCF m => [String] -> m ()
bcRunFile files = do
        when (length files /= 1) $
            failPCF "Debe especificarse exactamente 1 archivo."
        file <- liftIO . bcRead . head $ files
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