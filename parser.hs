module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

-- All possible lisp values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> String ("No match: \n" ++ show err)
    Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return (List [Atom "quote", x])

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseEscapedChars :: Parser Char
parseEscapedChars = do
    char '\\'
    x <- oneOf "\\\""
    return x

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ parseEscapedChars <|> (noneOf "\\\"")
    char '"'
    return $ String x

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space