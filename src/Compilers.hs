{-|
Module      : Compilers
Description : Scripts de compilacion de PCF.
Copyright   : (c) Román Castellarin, Sebastián Zimmermann, Mauro Jaskelioff, Guido Martínez, 2020-2021.
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

-- | Opciones de compilacion
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

-- | Ejecuta un parser, levanta una excepcion si hay error
parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r  

-- | Remueve el espacio blanco cuando termina la cadena
trim :: String -> String 
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Lee el contenido del archivo
-- nothrow: en caso de error imprime el resultado y retorna cadena vacia
openFilePCF :: MonadPCF m => String -> m String
openFilePCF filename = do
    printPCF $ "Abriendo " ++ filename ++ "..."
    liftIO $ catch (readFile filename)
             (\e -> do let err = show (e :: IOException)
                       hPutStr stderr $ "No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++ "\n"
                       return "")

-- | Carga lista de archivos en memoria
loadFiles :: MonadPCF m => Bool -> [String] -> m ()
loadFiles showCode = mapM_ $ \filename -> do
        modify (\s -> s { lfile = filename, inter = False })
        loadFile showCode filename

-- | Carga archivo en memoria
loadFile ::  MonadPCF m => Bool -> String -> m ()
loadFile showCode filename = do
    -- lee contenido
    contents <- openFilePCF filename
    decls <- parseIO filename program contents
    -- muestra codigo base
    when showCode $ do
        printPCF $ "\nBASE CODE " ++ filename ++ "\n" 
        printPCF $ intercalate "\n" $ show <$> decls
    -- typecheck y elaboracion
    mapM_ handleDecl decls

-- | Procesa declaraciones y las guarda en memoria 
handleDecl ::  MonadPCF m => SDecl STerm -> m ()
handleDecl decl = do
  d_elab <- elabDecl decl -- elaboracion
  forM_ d_elab $ \d -> do
      tcDecl d   -- typecheck
      addDecl d  -- guardado

typechecking :: MonadPCF m => [String] -> m ()
typechecking files = do 
    loadFiles False files
    decls <- glb <$> get
    when (null decls) $
        failPCF "Sin declaraciones."
    printPCF "El programa tipa correctamente."

-- | Compilacion general 
compilation :: MonadPCF m => CompilationOptions -> [String] -> m ()
compilation opts files = do
    -- lee archivo
    loadFiles (showBase opts) $ trim <$> files
    -- busca declaraciones procesadas
    ds_elab <- reverse . glb <$> get
    when (null ds_elab) $
        failPCF "Sin declaraciones."
    -- muestra codigo desazucarado
    when (showDesugar opts) $
        printPCF $ ("\nDESUGARED CODE\n" ++) $ intercalate "\n" $ show <$> ds_elab
    -- optimizaciones
    let optimizer = InlineExpansion.optimize 
                  . DeadCode.optimize 
                  . ConstantFolding.optimize
    let ds_opt = iterate optimizer ds_elab !! nOpt opts
    -- muestra codigo optimizado
    when (showOptimized opts) $
        printPCF $ ("\nOPTIMIZED CODE\n" ++) $ intercalate "\n" $ show <$> ds_opt
    -- closure conversion
    let ds_closure = runCC ds_opt
    -- muestra clausuras
    when (showClosures opts) $
        printPCF $ ("\nCLOSURE CONVERSIONS\n" ++) $ intercalate "\n" $ show <$> ds_closure
    -- canonization
    let ds_canon = runCanon ds_closure
    -- muestra clausuras
    when (showCanonized opts) $
        printPCF $ "\nCANONIZED CODE\n" ++ show ds_canon
    -- representation intermedia (IR)
    let ds_ir = codegen ds_canon
    -- muestra IR
    when (showIR opts) $
        printPCF $ "\nINTERMEDIATE REPRESENTATION\n" ++ show ds_ir
    -- traduccion LLVM
    let ds_llvm = ppllvm ds_ir
    liftIO $ TIO.writeFile "output.ll" ds_llvm
    -- muestra LLVM
    when (showLLVM opts) $
        printPCF $ "\nLLVM PROGRAM\n" ++ (read . show) ds_llvm
    -- compilacion LLVM 
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