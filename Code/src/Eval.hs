module Eval where

import Data
import Expr
import BSTVariables
import Command
import Data.Fixed

-- |The 'eval' function evaluates an expression given the current variables and returns the value or a string error message.
eval :: Node -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either String Value

-- evaluating values
eval vars (Val x) = Right x

-- evaluating variables
eval vars (Var a) = do v <- getNode vars a
                       return (value v)

-- evaluating addition
eval vars (Add x y) = do x' <- eval vars x
                         y' <- eval vars y
                         return (x' + y')

-- evaluating subtraction
eval vars (Sub x y) = do x' <- eval vars x
                         y' <- eval vars y
                         return (x' - y')

-- evaluating multiplication
eval vars (Mult x y) = do x' <- eval vars x
                          y' <- eval vars y
                          return (x' * y')

-- evaluating division
eval vars (Div x y) = do x' <- eval vars x
                         y' <- eval vars y
                         if y' /= (Int 0) && y' /= (Float 0) -- check for divide by 0
                                           then Right (x' / y')
                                           else Left "Can't divide by 0."

-- evaluating absolute values
eval vars (Abs x) = do x' <- eval vars x
                       return (abs x')

-- evaluating modulo
eval vars (Mod x y) = do x' <- eval vars x
                         y' <- eval vars y
                         return (mod x' y')

-- evaluating exponentials
eval vars (Pow x y) = do x' <- eval vars x
                         y' <- eval vars y
                         if y' >= (Float 0) || y' >= (Int 0) -- check for negative exponent
                           then Right (x' ** y')
                           else Left "Invalid exponent."
