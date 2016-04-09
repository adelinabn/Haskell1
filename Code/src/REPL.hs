module REPL where

import Parsing
import Data
import Expr
import BSTVariables
import Command
import Eval
import System.Environment
import Control.Exception
import System.Console.Haskeline
import Control.Monad.Trans

-- | Stores all the current state information.
data State = State { vars       :: Node,        -- ^ variable store
                     funcs      :: Node,        -- ^ function store
                     numCalcs   :: Int,         -- ^ number of calculations
                     history    :: [Command],   -- ^ command history
                     fileOutput :: String,      -- ^ calculator response to the lines of input
                     fileInput  :: [String],    -- ^ lines of input from external file
                     fileMode   :: Bool,        -- ^ flag to indicate whether program should read from/write to external file
                                                -- ^ rather than to the console
                     filepath   :: String,      -- ^ path of the file to read instructions from
                     toRepeat   :: ToRepeat     -- ^ command to be repeated, nr of times
                    }
  deriving Show

-- | Stores a repeat command.
data ToRepeat = ToRepeat { command :: Command,  -- ^ command to repeat
                           times :: Int         -- ^ number of times to repeat
                         }
  deriving Show

-- | Initialises a new starting state.
initState :: State
initState
  = State (VarNode { name = "it",
                      value = (Int 0),
                      left = Nil,
                      right = Nil     })                      -- BST of variables
          (FuncNode { name = "root",
                       argn = [],
                       expr = (Val (Int 0)),
                       left = Nil,
                       right = Nil         })                 -- BST of functions
          0                                                   -- numcalcs
          []                                                  -- history
          []                                                  -- fileoutput
          []                                                  -- fileinput
          False                                               -- filemode
          []                                                  -- filepath
          (ToRepeat { command = (Hist 0), times = 0 })        -- command to be repeated

-- | Update the variables of a state.
-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value.
updateVars :: Name -> Value -> Node -> Node
updateVars vname x vars = insertNode vars (VarNode { name = vname,
                                                      value = x,
                                                      left = Nil,
                                                      right = Nil })

-- | Update the functions of a state.
updateFuncs :: Name -> [Name] -> Expr -> Node -> Node
updateFuncs fname args e funcs = insertNode funcs (FuncNode { name = fname,
                                                         argn = args,
                                                         expr = e,
                                                         left = Nil,
                                                         right = Nil    })

-- | Return a new set of variables with the given name removed.
dropVar :: Name -> Node -> Node
dropVar name vars = removeNode vars name

-- | Add a command to the command history in the state.
addHistory :: State -> Command -> State
addHistory st command = st { history = history st ++ [command] }

-- | Removes the last element from a list, returning the updated list
dropLast :: [a] -> [a]
dropLast xs =  take (length xs-1) xs

-- | Generates the current prefix according to the number of calculations performed.
numCalcsPrefix :: State -> String
numCalcsPrefix st = "\n"++ show (numCalcs st) ++ ">\t"


--------------------------------- Processing -----------------------------------

-- | Process a command according to the current state.
process :: State -> Command -> InputT IO ()

-- Adds a variable & its associated value to the list of variables (stored as a BST)
process st (Set var e)
     = do let result = eval (vars st) e
          case result of
            Right r -> do let st'= st {vars = updateVars "it" r (updateVars var r (vars st))}
                          outputMsg st' "OK"
            Left msg -> outputMsg st msg

-- Removes a variable with a given name from the list of variables
process st (Remove varName)
    = if varName == "it"
        then outputMsg st "Cannot remove special variable it."
        else do let st'= st {vars = removeNode (vars st) varName}
                outputMsg st' "OK"

-- Displays the result of evaluating a (possibly nested) expression & updates variable 'it' with that result
process st (Eval e)
    = do let result = eval (vars st) e
         case result of
           Right r -> do let st' = st { vars = updateVars "it" r (vars st) }
                         outputMsg st' (show r)
           Left msg ->  outputMsg st msg

-- Re-processes a history command given its index (if valid)
process st (Hist i)
    = if (i < length (history st)-1) && (i >= 0)
        then process st (history st !! i)
        else outputMsg st "Invalid history index!"

-- Shows the history command given its index
process st (Show i)
    = if (i < length (history st)-1) && (i >= 0)
        then outputMsg st (show (history st !! i))
        else outputMsg st "Invalid history index!"

-- Exits the calculator
process st (Quit)
    = if fileMode st
        then repl st { fileOutput = fileOutput st ++ numCalcsPrefix st ++ "CALC:\t" ++ "Retype command at console to quit.\n" }
        else do outputStr "Leaving calculator.\n"
                return ()

-- Reads an external file and stores the commands read in the fileInput attribute of state
process st (ProcessFile p)
    = if fileMode st
        then outputMsg st "Already processing: current file."  -- returns, does not support :process command when read from external file
        else do outputStr ("\n   \tReading file ... " ++ show p ++ "\n")
                result <- liftIO (try (readFile p)) :: InputT IO (Either SomeException String)
                case result of
                  Left ex  -> do outputStr $ "\n   \tFailed to read:\n" ++ show ex ++"\n"
                                 repl st
                  Right str -> do let nonEmptyLines = removeEmptyLines (lines str)
                                  if nonEmptyLines /= []
                                    then do -- 'load' the fileinput array with lines read from external file
                                            -- & write a title to file output
                                            let st'= st { fileOutput = fileOutput st ++ "  \tCALC:\tResults of processing file:\n  \t" ++ show p ++ "\n",
                                                          fileInput = nonEmptyLines, fileMode = True, filepath = p , numCalcs = numCalcs st-1 }
                                            -- we don't know whether the first command read from file will be valid
                                            -- and will let repl_file decide whether to increment numcalcs back or not
                                            outputStr $ "\n   \tOK! Result will be saved with extension : _result.txt\n"
                                            replFile st'
                                    else do outputStr $ "\n   \tNothing to process inside:\n   \t" ++ show p ++ "\n"
                                            repl st

-- Repeats a command 'cmd'
-- 'n' number of times
process st (Repeat cmd n)
    = if n > 0
        then do let t = if times (toRepeat st) == 0
                          then n
                          else ((times (toRepeat st) + 1 )*n)
                          --if a repeat command is processed inside another
                          -- repeat command, will multiply times
                repl (st { toRepeat = ToRepeat{command = cmd, times = t} })
        else outputMsg st "Cannot repeat command zero times."

-- Define function
process st (DefFunc fName argn expression)
  = do let st' = st { funcs = updateFuncs fName argn expression (funcs st) }
       outputMsg st' "OK"

-- Call function
process st (CallFunc fName params)
  = do let f = getNode (funcs st) fName
       case f of
         Right function -> do let args = argn function -- get arguments
                              let e = expr function -- get expression
                              if (length params) == (length args) -- check number of arguments
                                then do let result = eval (makeTree args params) e
                                        case result of
                                          Right r -> do let st' = st { vars = updateVars "it" r (vars st) }
                                                        outputMsg st' (show r)
                                          Left msg ->  outputMsg st msg
                                else outputMsg st "Incorrect number of parameters!"
         Left msg -> outputMsg st msg

-- | Make a variable tree from a list of names and values.
makeTree :: [Name] -> [Value] -> Node
makeTree [] [] = Nil
makeTree (n:[]) (v:[]) = (VarNode n v Nil Nil )
makeTree (n:ns) (v:vs) = insertNode (makeTree ns vs) (makeTree (n:[]) (v:[]))


------------------------ Read - Evaluate - Print loops -------------------------

-- | Main read, evaluate, print loop.
-- Takes input from console, processes it and writes to console OR processes a command that should be repeated
repl ::  State -> InputT IO ()
repl st
    = if times (toRepeat st) == 0
        then if fileMode st
          then replFile st -- redirects to replFile cycle
          else do inp <- getInputLine ("\n"++ (numCalcsPrefix st)) -- show (numCalcs st) ++ " > ")
                  case inp of
                    Just str -> case parse pCommand str of
                                  [(cmd, "")] -> -- Must parse entire input
                                          do let st'= addHistory st cmd
                                             process st'{numCalcs=numCalcs st+1} cmd
                                  _ -> do outputMsg st ("Command not found!")
                    Nothing -> process st (Quit)
        else do let st'= st{ toRepeat = ToRepeat -- repeat the command whose 'times' attribute is still /= 0
                                          {command = command (toRepeat st),
                                          times = times (toRepeat st) -1}}
                process st' (command (toRepeat st'))

-- | Read, evaluate, print loop for files.
-- Takes input from (and writes to) file rather than console
replFile :: State -> InputT IO ()
replFile st
    = if fileInput st == []    -- if there is nothing to process from external file
        then do let st' = st { fileMode = False, numCalcs = (numCalcs st)+1 }
                let path = take (length (filepath st)-4) (filepath st) ++"_result.txt"
                -- if there is something to output, write it to _result.txt
                if fileOutput st == []
                  then repl st'
                  else do liftIO (writeFile path (fileOutput st'))
                          ans <- getInputLine "\nWould you like to view the results (y/n)?\n"
                          case ans of
                            Just str -> if str == "y"
                                          then do outputStr ("\nOutput:\n"++(fileOutput st))
                                                  repl st' { fileOutput = [] }
                                          else repl st' { fileOutput = [] }
                            Nothing -> repl st' { fileOutput = [] }
        else do let inp = head (fileInput st)   -- process first instruction in the queue
                let st' = st{ fileInput = tail (fileInput st) }
                case parse pCommand inp of
                   [(cmd, "")] ->  do let s = addHistory st'{numCalcs =numCalcs st'+1} cmd
                                      let st'' = s { fileOutput = fileOutput s ++ numCalcsPrefix s ++ "USER:\t" ++ inp ++ "\n"}
                                      process st'' cmd
                   _ -> do let st'' = st' { fileOutput = fileOutput st' ++ "\nUSER:\t" ++ inp ++ "\nCALC:\tCommand not found\n"}
                           replFile st''


----------------------------------- Printing -----------------------------------

-- | Output a message to the console or a file depending on the fileMode of the state and call repl.
outputMsg :: State -> String -> InputT IO ()
outputMsg st msg
  = if times (toRepeat st) == 0
      then if fileMode st
             then repl (st {fileOutput = fileOutput st ++ numCalcsPrefix st ++ "CALC:\t" ++ msg ++ "\n"})
             else do  outputStr $ "\n" ++ msg ++ "\n"
                      repl st
      else repl st

-- | Remove empty strings from beginning of list of strings.
removeEmptyLines :: [String] -> [String]
removeEmptyLines [] = []
removeEmptyLines xs
  = if head xs == ""
      then removeEmptyLines ( tail xs)
      else case parse space (head xs) of
             [(spaces, "")] -> removeEmptyLines ( tail xs)
             -- removes spaces at the beginning of the line
             [(spaces, nonSpaces)] -> nonSpaces : (removeEmptyLines (tail xs))
             _              -> (head xs):(removeEmptyLines (tail xs))
