module Main where

import System.Exit (die)

import AST
import Desugar
import Eval
import Parser
import Pretty
import TypeCheck


main :: IO ()
main = do
  source <- getContents
  prog1 <- case parseProgram "<stdin>" source of
             Left err -> die err
             Right prog -> return prog
  prog2 <- case desugarProgram prog1 of
             Left err -> die err
             Right prog -> return prog
  prog3 <- case typeCheck prog2 of
             Left err -> do
               putStr (ppProgram prog2)
               die (ppTypeErr err)
             Right prog -> return prog
  putStrLn (ppProgram prog3)

  putStrLn $ ppValue 0 (evalProgram prog3) " : " ++ ppTyp 0 (typeOf (progMain prog3))
