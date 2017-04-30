module Parser where

import Types

import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError (Parser err)
    Right val -> return val

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