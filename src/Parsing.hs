module Parsing (
    parseLine
) where

import Text.Parsec hiding (Line)
import Text.Parsec.String hiding (Line)

number :: Parser String 
number = many1 digit

plus :: Parser String 
plus = char '+' *> number

minus :: Parser String 
minus = (:) <$> char '-' <*> number

integer :: Parser String 
integer = plus <|> minus <|> number

double_parser :: Parser Double 
double_parser = fmap rd $ (++) <$> integer <*> decimal
    where rd      = read :: String -> Double
          decimal = option "" $ (:) <$> char '.' <*> number

inner_tuple :: Parser (Double, Double)
inner_tuple = do
    first <- double_parser
    sep_with_space ','
    second <- double_parser
    return (first, second)

tuple :: Parser (Double, Double)
tuple = do
    char '('
    result <- inner_tuple
    char ')'
    return result

list :: Parser [(Double, Double)]
list = do
    res <- many1 (tuple <* optional (sep_with_space ','))
    return res

sep_with_space :: Char -> Parser Char
sep_with_space c = spaces >>  char c <* spaces

line_parsing :: Parser ([(Double, Double)], [[(Double, Double)]])
line_parsing = do
    many digit
    string ": "
    first <- list 
    second <- option [] (char '#' *> many (list <* optional (sep_with_space ';')))
    return (first, second)

parseLine :: String 
            -> Either ParseError ([(Double, Double)], [[(Double, Double)]])
parseLine str = parse line_parsing "" str