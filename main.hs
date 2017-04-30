module Main where

import Parser
import Evaluator
import Errors

import Control.Monad
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    evaled <- return (liftM show (readExpr (args !! 0) >>= eval))
    putStrLn (extractValue (trapError evaled))

-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing