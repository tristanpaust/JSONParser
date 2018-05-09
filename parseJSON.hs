module ParseJSON where

import Text.Parsec
import Text.Parsec.String

type Name = String -- This is sort of the label of the object / sub-object

data JSONData = JSONData Name JSON -- A JSON object has a label (Name) which is followed by colon + curly braces and data
  deriving Show

data JSON = Object [JSONData] -- The whole object, as well as sub-objects
          | Boolean Bool 
          | Str String 
          | Number Double
          | Array [JSON] -- JSON Arrays repeat any of the other types / itself
          | None -- In case there is an empty node in the file
  deriving Show

json :: Parser JSON
json = object <|> boolean <|> str <|> number <|> array <|> none

object :: Parser JSON
object = do
    spaces
    char '{'
    spaces
    kind <- parseJSONData `sepBy` (spaces >> char ',' >> spaces)-- This gives the entire line, e.g. "test-attribute" : value
    spaces
    char '}'
    spaces
    return (Object kind)


parseJSONData :: Parser JSONData
parseJSONData = do 
    name <- getName
    value <- (boolean
         <|>  str
         <|>  number
         <|>  array
         <|>  none
         <|>  object)
    return (JSONData name value)

getName :: Parser Name
getName = do 
    spaces
    name <- parseStr
    spaces
    char ':'
    spaces
    return name

boolean :: Parser JSON
boolean = do
    spaces
    boolean <- string "true" <|> string "false"
    spaces
    if boolean == "true" 
        then return (Boolean True)
        else return (Boolean False)

str :: Parser JSON
str = do
    spaces
    string <- parseStr
    spaces
    return (Str string)

parseStr = between (char '\"') (char '\"') (many $ noneOf "\"" <|> try (string "\"\"" >> return '"'))

number :: Parser JSON
number = do
    spaces
    fullNumber <- many (digit <|> oneOf ".-") -- Number can be int, which is only digits, or a floating point number of a negative number
    return (Number (read fullNumber))

array :: Parser JSON
array = do
    spaces
    char '['
    arr <- sepBy (object
        <|> array
        <|> boolean
        <|> none
        <|> str
        <|> number) (spaces >> char ',' >> spaces)
    spaces 
    char ']'
    spaces
    return (Array arr)

none :: Parser JSON
none = do
    spaces
    string "null"
    spaces
    return None

runTest = do 
    test <- readFile "test.json"
    parseTest json test