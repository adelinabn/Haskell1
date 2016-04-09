module Command where

import Parsing
import Data
import Expr

-- | Stores a parsed command.
data Command = Eval Expr                -- ^ evaluate expression
             | Set Name Expr            -- ^ add variable to memory
             | Remove Name              -- ^ remove variable from memory
             | Hist Int                 -- ^ repeat command fiven history index
             | Quit                     -- ^ exit calculator
             | ProcessFile String       -- ^ process instructions read from a file given its path
             | Repeat Command Int       -- ^ repeat command <number of times>
             | Show Int                 -- ^ show command given its index in history
             | DefFunc Name [Name] Expr -- ^ defines a function
             | CallFunc Name [Value]    -- ^ calls a function
  deriving Show

-- | Parses an entire command.
pCommand :: Parser Command
pCommand = pSetVariableCmd
         ||| pProcessFileCmd
           ||| pHistoryCmd
             ||| pQuitCmd
               ||| pRepeatCmd
                 ||| pShowHistoryCmd
                   ||| pRemoveVariableCmd
                     ||| pDefineFunctionCmd
                       ||| pCallFuncCmd
                         ||| do e <- pExpr
                                return (Eval e)

-- | Parses a 'Set' command.
pSetVariableCmd :: Parser Command
pSetVariableCmd = do t <- identifier
                     character '='
                     e <- pExpr
                     return (Set t e)

-- | Parses a 'Remove' command.
pRemoveVariableCmd :: Parser Command
pRemoveVariableCmd = do symbol ":remove"
                        i <- identifier
                        return (Remove i)

-- | Parses a 'Hist' command.
pHistoryCmd :: Parser Command
pHistoryCmd = do character '!'
                 n <- natural
                 return (Hist (fromIntegral n))

-- | Parses a 'Quit' command.
pQuitCmd :: Parser Command
pQuitCmd = do symbol ":q"
              return (Quit)

-- | Parses a 'ProcessFile' command.
pProcessFileCmd :: Parser Command
pProcessFileCmd = do symbol ":process"
                     p <- path
                     return (ProcessFile p)

-- | Parses a 'Repeat' command.
pRepeatCmd :: Parser Command
pRepeatCmd = do symbol ":repeat"
                cmd <- pCommand
                n <- natural
                symbol "times"
                return (Repeat cmd (fromIntegral n))

-- | Parses a 'Show' command.
pShowHistoryCmd :: Parser Command
pShowHistoryCmd = do symbol ":show"
                     character '!'
                     n <- natural
                     return (Show (fromIntegral n))

-- | Parses a 'Func' command.
pDefineFunctionCmd :: Parser Command
pDefineFunctionCmd = do symbol ":func"
                        name <- identifier
                        character '('
                        argn <- arguments
                        character ')'
                        character '='
                        expression <- pExpr
                        return (DefFunc name argn expression)

-- | Parses a 'Call' command.
pCallFuncCmd :: Parser Command
pCallFuncCmd = do symbol ":call"
                  name <- identifier
                  character '('
                  ps <- parameters
                  character ')'
                  return (CallFunc name (floatsToValues ps))

-- | Helper function to convert a list of floats to a list of parsed values.
floatsToValues :: [Float] -> [Value]
floatsToValues [] = []
floatsToValues (x:xs) = if isInt x
                          then (Int (round x)):(floatsToValues xs)
                          else (Float x):(floatsToValues xs)
