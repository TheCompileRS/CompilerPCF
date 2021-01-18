{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Roman Castellarin, Sebastián Zimmermann, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)
import Data.Maybe ( catMaybes ) 

--import Control.Monad
import Control.Monad.Trans
import Data.List (intercalate, nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
--import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab, elabDecl )
--import Eval ( eval )
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import CEK (search, valToTerm)
import Options.Applicative
import Bytecompile (runBC, bcRead, bcWrite, bytecompileModule)
import ClosureConv (runCC)
import CanonConv (runCanon)
import InstSel (codegen)
import System.Process (system)

import LLVM.Pretty
import qualified Data.Text.Lazy.IO as TIO

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConv

-- | Parser de banderas
parseMode :: Parser Mode
parseMode = flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos")
            <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
            <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM")
            <|> flag' ClosureConv (long "cc" <> help "Conversion de clausuras")
            <|> flag Interactive Interactive ( long "interactive" <> short 'i'
                                               <> help "Ejecutar en forma interactiva" )
            
-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
                ( fullDesc
                  <> progDesc "Compilador de PCF"
                  <> header "Compilador de PCF de la materia Compiladores 2020" )
    
    go :: (Mode,[FilePath]) -> IO ()
    go (Interactive,files) = do runPCF (runInputT defaultSettings (main' files))
                                return ()
    go (Typecheck, files) = undefined
    go (Bytecompile, files) = do runPCF $ bcCompileFiles files
                                 return ()
    go (Run,[file]) = do runPCF $ bcRunFile file
                         return ()
    go (ClosureConv,files) = do  runPCF $ compileClosures files 
                                 return ()
    go _ = return ()

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
    decls <- catMaybes <$> mapM elabDecl sdecls 
    --printPCF $ intercalate "\n" $ show <$> runCC decls
    let irdecls = (codegen . runCanon. runCC) decls
    liftIO $ TIO.writeFile "output.ll" (ppllvm irdecls)
    let commandline = "clang -Wno-override-module output.ll src/runtime.c -lgc -o prog"
    liftIO $ system commandline
    return ()
    

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

prompt :: String
prompt = "PCF> "
          
main' :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
main' args = do
        lift $ catchErrors $ compileFiles args
        s <- lift $ get
        when (inter s) $ liftIO $ putStrLn
          (  "Entorno interactivo para PCF1.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop  
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (flip when loop) b
 
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

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

handleDecl ::  MonadPCF m => SDecl STerm -> m ()
handleDecl decl = do 
  decl' <- elabDecl decl
  case decl' of
    Nothing -> return ()
    Just (Decl p x ty tt) -> do
        tcDecl (Decl p x ty tt)
        --te <- eval tt
        te <- liftM valToTerm $ search tt [] []
        addDecl (Decl p x ty te)

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> handleDecl d
      Right t -> handleTerm t

handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         --te <- eval tt
         te <- liftM valToTerm $ search tt [] [] 
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    ex <- elab x'
    t  <- case x' of 
           (BV _ f) -> maybe ex id <$> lookupDecl f
           _        -> return ex  
    printPCF "STerm:"
    printPCF (show x')
    printPCF "\nTerm:"
    printPCF (show t)

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)

-- --------------------------
