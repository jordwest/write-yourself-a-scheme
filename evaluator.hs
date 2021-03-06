module Evaluator where

import Types

import Control.Monad
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError (NotFunction "Unrecognized primitive functions args" func))
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []     = throwError (NumArgs 2 [])
numericBinop op x@[_]  = throwError (NumArgs 2 x)
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
                       in if null parsed
                           then throwError (TypeMismatch "number" (String n))
                           else return (fst (parsed !! 0))

unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError (TypeMismatch "number" notNum)