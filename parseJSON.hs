module ParseJSON where

import Text.Parsec
import Text.Parsec.String

type Name = String -- This is sort of the label of the object / sub-object

data JSONObj = JSONObj Name JSON -- A JSON object has a label (Name) which is followed by colon + curly braces and data
  deriving Show

data JSON = Object -- The whole object, as well as sub-objects
          | Boolean Bool 
          | Str String 
          | Number Int -- This should probably be Double or something
          | Array [JSON] -- JSON Arrays repeat any of the other types / itself
          | None -- In case there is an empty node in the file
  deriving Show

json :: Parser JSON
json = object <|> boolean <|> str <|> number <|> array <|> none

object :: Parser JSON
object = do
    return None

boolean :: Parser JSON
boolean = do
    return None

str :: Parser JSON
str = do
    return None

number :: Parser JSON
number = do
    return None

array :: Parser JSON
array = do
    return None

none :: Parser JSON
none = 
    return None