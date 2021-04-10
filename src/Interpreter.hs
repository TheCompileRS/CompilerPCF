{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Interpreter
Description : Entorno interactivo de PCF.
Copyright   : (c) Román Castellarin, Sebastián Zimmermann, Mauro Jaskelioff, Guido Martínez, 2021.
License     : GPL-3
Stability   : experimental

-}

module Interpreter where

import Control.Monad.Catch (MonadMask)
import MonadPCF
import System.Console.Haskeline (InputT, getInputLine)
import Data.List (intercalate, nub, isPrefixOf )
import Global (GlEnv(..))
import Data.Char (isSpace)
import Lang
import PPrint (ppTy, pp)
import Elab (elab)
import Compilers (parseIO, loadFile, loadFiles, handleDecl)
import Parse (declOrTm, tm)
import CEK (valToTerm, search)
import TypeChecker (tc)
import Data.Maybe (fromMaybe, isNothing)



prompt :: String
prompt = "PCF> "

interpreter :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
interpreter args = do
        r <- lift . catchErrors $ loadFiles False args
        when (isNothing r) $ liftIO $ 
          putStrLn "Lectura de archivos abortada..."
        s <- lift get
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
                       maybe loop (`when` loop) b

-- | Comandos del modo interprete
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
  =  if ":" `isPrefixOf` x then
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
                                   intercalate ", " ([ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
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
     "<expr>                           evaluar la expresión\n" ++
     "let <var> : <type> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
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
       Browse ->  do  printPCF (unlines (reverse (nub (map declName glb))))
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> loadFile False f
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
         te <- valToTerm <$> search tt [] []
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    ex <- elab x'
    t  <- case x' of
           (BV _ f) -> fromMaybe ex <$> lookupDecl f
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