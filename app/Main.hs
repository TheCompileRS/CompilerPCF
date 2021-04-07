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
import Compilers (bcCompileFiles, bcRunFile, compileClosures)

-- | Modos de ejecución
data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConv

-- | Parser de banderas
parseMode :: Parser Mode
parseMode = flag' Typecheck   (long "typecheck"   <> short 't' <> help "Solo chequear tipos")
        <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
        <|> flag' Run         (long "run"         <> short 'r' <> help "Ejecutar bytecode en la BVM")
        <|> flag' ClosureConv (long "cc"                       <> help "Conversion de clausuras")
        <|> flag  Interactive Interactive ( long "interactive" <> short 'i'
                                           <> help "Ejecutar en forma interactiva" )

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
    go (Interactive,files) = do runPCF (runInputT defaultSettings (interpreter files))
                                return ()
    go (Typecheck, files) = undefined
    go (Bytecompile, files) = do runPCF $ bcCompileFiles files
                                 return ()
    go (Run,[file]) = do runPCF $ bcRunFile file
                         return ()
    go (ClosureConv,files) = do  runPCF $ compileClosures files
                                 return ()
    go _ = return ()


