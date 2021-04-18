{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Román Castellarin, Sebastián Zimmermann, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, runInputT )

import MonadPCF
import Options.Applicative

import Interpreter (interpreter)
import Compilers (bcCompileFiles, bcRunFile, compilation, CompilationOptions(..), typechecking)

-- | Modos de ejecución
data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Byterun
          | Compilation CompilationOptions

-- | Parser de banderas
parseMode :: Parser Mode
parseMode = flag' Typecheck   (long "typecheck"   <> short 't' <> help "Solo chequear tipos")
        <|> flag' Bytecompile (long "bytecompile" <> short 'b' <> help "Compilar a la BVM")
        <|> flag' Byterun     (long "run"         <> short 'r' <> help "Ejecutar bytecode en Haskell BVM")
        <|> flag' Interactive (long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
        <|> Compilation <$> parseCompilation

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))

-- | Main
main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
                ( fullDesc
                  <> progDesc "Compilador de PCF"
                  <> header "Compilador de PCF de la materia Compiladores 2020" )

    go :: (Mode,[FilePath]) -> IO ()
    go (Interactive,files)    = void . runPCF $ runInputT defaultSettings (interpreter files)                         
    go (Typecheck, files)     = void . runPCF $ catchErrors (typechecking files)
    go (Bytecompile, files)   = void . runPCF $ bcCompileFiles files
    go (Byterun, files)       = void . runPCF $ bcRunFile files
    go (Compilation c, files) = void . runPCF $ catchErrors (compilation c files)

-- | Parser de opciones de compilación
parseCompilation :: Parser CompilationOptions
parseCompilation =  flag CompilationOptions CompilationOptions
         (short 'c' <> long "compile"   <> help "compilacion completa")
      <*> switch (long "show-base"      <> help "show parser base output")
      <*> switch (long "show-desugar"   <> help "show desugared output")
      <*> switch (long "show-optimized" <> help "show optimized output")
      <*> switch (long "show-closures"  <> help "show closures output")
      <*> switch (long "show-canonized" <> help "show canonized output")
      <*> switch (long "show-ir"        <> help "show intermediate repr. output")
      <*> switch (long "show-llvm"      <> help "show LLVM output")
      <*> option auto (long "n-opt" <> value 20 <> help "n. of optimization rounds")
