module Main where

import Parser
import Evaluator

import System.Environment

main :: IO ()
main = getArgs >>= (print . eval . readExpr . head)

-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing