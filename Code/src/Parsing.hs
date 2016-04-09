{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative, empty, (<|>))

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])
--to give a new name , so we can replace Parser with just 'P'
{-
instance Functor Parser where
   fmap = liftM
instance Applicative Parser where
   pure  = return
   (<*>) = ap
-}
instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)
{-
instance Alternative Parser where
   empty = mzero
   (<|>) = mplus
-}
instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

--added
latin                         :: Parser Char
latin                         = sat isLatin1

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                       :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

--added
commaseparated                :: Parser a -> Parser a
commaseparated p              = do char ','
                                   v <- p
                                   return v

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Integer
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Integer
int                           = do char '-'
                                   n <- nat
                                   return (-n)
                                 ||| nat

--added
real                          :: Parser Float
real                          = do xs <- many1 digit
                                   char '.'
                                   ds <- many1 digit
                                   return (read (xs ++ ['.'] ++ ds))

--added
flo                           :: Parser Float
flo                           = do char '-'
                                   f <- real
                                   return (negate f)
                                 ||| real

--added
num                           :: Parser Float
num                           = flo
                              ||| do xs <- many1 digit
                                     return (read xs)

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

--added
str                           :: Parser String
str                           = do char '\"'
                                   s <- many alphanum --change to latin
                                   char '\"'
                                   return s

{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Integer
natural                       =  token nat

integer                       :: Parser Integer
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

--modified
path                          :: Parser String
path                          = do l <- letter
                                   m <- many item
                                   return (l:m)
                                 ||| do string "./"
                                        m <- many item
                                        return ("./" ++ m)
                                      ||| do string "../"
                                             m <- many item
                                             return ("../" ++ m)

--added
float                         :: Parser Float
float                         = token flo

--added
character                     :: Char -> Parser Char
character x                   = token (char x)

--added
strvalue                      :: Parser String
strvalue                      = token str

--added
arguments                     :: Parser [String]
arguments                     = do i <- identifier
                                   is <- many (commaseparated identifier)
                                   return (i:is)
                                  ||| return []

--added
number                        :: Parser Float
number                        = token num

--added
parameters                    :: Parser [Float]
parameters                    = do i <- number
                                   is <- many (commaseparated number)
                                   return (i:is)
                                  ||| return []
