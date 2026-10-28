module Main where

import LexProgram
import ParProgram
import AbsProgram
import Interpreter

import ErrM

import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> inter stdin
    [arg] -> withFile arg ReadMode inter
    _     -> printError "0 or 1 command line arguments expected"

printError :: String -> IO ()
printError = hPutStrLn stderr

inter :: Handle -> IO ()
inter h = do
  s <- hGetContents h
  case pProgram (myLexer s) of
    Ok e    -> do
      ans <- interpreter e
      case ans of
        (Left e)  -> printError $ show e
        (Right _) -> return ()
    Bad msg -> do printError msg
