{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, Roman Castellarin, Sebastián Zimmermann 2020.
License     : GPL-3
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentStart  = "{-",
         commentEnd    = "-}",
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else", 
                          "succ", "pred", "ifz", "Nat", "rec", "in"],
         reservedOpNames = ["->",":","="],
         caseSensitive = True
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunTy x y))
      <|> tyatom
          
const :: P Const
const = CNat <$> num

unaryOp :: P STerm
unaryOp = do
  i <- getPos
  foldr (\(w, r) rest -> try (do 
                                 reserved w
                                 a <- atom
                                 return (r a)) <|> rest) parserZero (mapping i)
  where
   mapping i = [
       ("succ", BUnaryOp i Succ)
     , ("pred", BUnaryOp i Pred)
    ]

atom :: P STerm
atom =     (flip BConst <$> const <*> getPos)
       <|> flip BV <$> var <*> getPos
       <|> parens tm

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         bs <- multibinding
         reservedOp "->"
         t <- tm
         return $ SLam i bs t

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = do  i <- getPos
          f <- atom
          args <- many atom
          return $ foldl (BApp i) f args

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         e <- tm
         return $ BIfZ i c t e

binding :: P (Name, Ty)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

multibinding :: P [([Name], Ty)]
multibinding = many1 $ parens $ 
        do  xs <- many1 var
            reservedOp ":"
            t  <- typeP
            return (xs, t)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         bs <- multibinding
         reservedOp "->"
         t <- tm
         return (BFix i f fty bs t)

-- | Parser de términos
tm :: P STerm
tm = app <|> lam <|> ifz <|> unaryOp <|> fix <|> try letFix <|> try letIn <|> letLam   

letIn :: P STerm
letIn = do 
     i <- getPos
     reserved "let"
     (v, ty) <- binding
     reservedOp "="
     t1 <- tm
     reserved "in"
     t2 <- tm
     return (SLet i v ty t1 t2)

letLam :: P STerm
letLam = do 
      i <- getPos
      reserved "let"
      f <- var
      bs <- multibinding
      reservedOp ":"
      fty  <- typeP
      reservedOp "="
      t1 <- tm
      reserved "in"
      t2 <- tm
      return (SLetLam i f fty bs t1 t2)

letFix :: P STerm
letFix = do 
      i <- getPos
      reserved "let"
      reserved "rec"
      f <- var
      bs <- multibinding
      reservedOp ":"
      fty  <- typeP
      reservedOp "="
      t1 <- tm
      reserved "in"
      t2 <- tm
      return (SLetFix i f fty bs t1 t2)

-- | Parser de declaraciones
decl :: P (Decl STerm)
decl = try declFix <|> try declLet <|> declLam 

declLet :: P (Decl STerm)
declLet = do 
      i <- getPos
      reserved "let"
      v <- var
      reservedOp ":"
      ty  <- typeP
      reservedOp "="
      t <- tm
      return (Decl i v ty t)

declLam :: P (Decl STerm)
declLam = do 
      i <- getPos
      reserved "let"
      f <- var
      bs <- multibinding
      reservedOp ":"
      ty  <- typeP
      reservedOp "="
      t <- tm
      return (Decl i f ty (SLam i bs t))

declFix :: P (Decl STerm)
declFix = do 
      i <- getPos
      reserved "let"
      reserved "rec"
      f <- var
      bs <- multibinding
      reservedOp ":"
      ty  <- typeP
      reservedOp "="
      t <- tm
      return (Decl i f ty (BFix i f ty bs t))

-- | Parser de programas (listas de declaraciones) 
program :: P [Decl STerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl STerm) STerm)
declOrTm =  try (Right <$> tm) <|> (Left <$> decl) 

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> (Either (Decl STerm) STerm)
parse s = case runP declOrTm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
