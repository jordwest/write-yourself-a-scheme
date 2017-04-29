module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parser = spaces >> symbol

readExpr :: String -> String
readExpr input = case parse parser "lisp" input of
    Left  err -> "No match: \n" ++ show err
    Right val -> "Found value"