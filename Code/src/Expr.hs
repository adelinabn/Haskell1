module Expr where

import Parsing
import Data

-- | Stores an Expression.
data Expr = Val Value
          | Sub Expr Expr
          | Add Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Var Name
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr
  deriving (Show, Eq)

-- | Parses an entire expression.
pExpr :: Parser Expr
pExpr = do t <- pTerm
           pLowPriority t

-- | Parses a low precedence operation (addition/subtraction).
pLowPriority :: Expr -> Parser Expr
pLowPriority e = pAddition e
               ||| pSubtraction e
                 ||| return e

-- | Parses an addition.
pAddition:: Expr -> Parser Expr
pAddition t1 = do character '+'
                  t2 <- pTerm
                  pLowPriority (Add t1 t2)

-- | Parses a subtraction.
pSubtraction :: Expr -> Parser Expr
pSubtraction t1 = do character '-'
                     t2 <- pTerm
                     pLowPriority (Sub t1 t2)

-- | Parses a term of a low precedence operation.
pTerm :: Parser Expr
pTerm = do x <- pFactor
           pHighPriority x

-- | Parses a high precedence operation.
pHighPriority :: Expr -> Parser Expr
pHighPriority e = pProduct e
                ||| pDivision e
                  ||| pModExpr e
                    ||| pExponentialExpr e
                      ||| return e

-- | Parses a multiplication.
pProduct :: Expr -> Parser Expr
pProduct f1 = do character '*'
                 f2 <- pFactor
                 pHighPriority (Mult f1 f2)

-- | Parses a division.
pDivision :: Expr -> Parser Expr
pDivision d1 = do character '/'
                  d2 <- pFactor
                  pHighPriority (Div d1 d2)

-- | Parses a modulo expression.
pModExpr :: Expr -> Parser Expr
pModExpr m1 = do character '%'
                 m2 <- pFactor
                 pHighPriority (Mod m1 m2)

-- | Parses an exponential expression.
pExponentialExpr :: Expr -> Parser Expr
pExponentialExpr e1 = do character '^'
                         e2 <- pFactor
                         pHighPriority (Pow e1 e2)

-- | Parses a factor of a higher precedence operation.
pFactor :: Parser Expr
pFactor = pFloat
        ||| pInt
          ||| pString
            ||| pVariable
              ||| pBracketedExpr
                ||| pAbsExpr

-- | Parses an integer value.
pInt :: Parser Expr
pInt = do i <- integer
          return (Val (Int i))

-- | Parses a float value.
pFloat :: Parser Expr
pFloat = do f <- float
            return (Val (Float f))

-- | Parses a String value.
pString :: Parser Expr
pString = do s <- strvalue
             return (Val (String s))

-- | Parses a variable identifier.
pVariable :: Parser Expr
pVariable = do v <- identifier
               return (Var v)

-- | Parses an expression in round brackets.
pBracketedExpr :: Parser Expr
pBracketedExpr = do character '('
                    e <- pExpr
                    character ')'
                    return e

-- | Parses an absolute value.
pAbsExpr :: Parser Expr
pAbsExpr = do character '|'
              g <- pExpr
              character '|'
              return (Abs g)
