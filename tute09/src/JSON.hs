-- | Implement a limited JSON parser.
--
-- Use the provided JsonValue type to create a parser for JSON values.
-- A good starting point is to consider how to parse the literal types
-- such as JTrue, JFalse, JNull, JInteger, and JString.
--
-- Make sure to refer to Instances.hs and Parser.hs for utility functions,
-- parsers, and parser combinators to help you.
--
-- **Reminders**
-- You must NOT manually construct the Parser type using the Parser constructor,
-- nor use the `parse` function to chain together parsers. You must also NOT
-- use the `reads` function or equivalent. The cases where this is the
-- intended solution have already been provided to you.
--
-- The README contains a list of useful functions and operators worth looking at.
--
-- see https://tgdwyer.github.io/parsercombinators/#a-parser-that-returns-an-adt
module JSON () where

import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2))
import Instances (Parser (..), int)
import Parser (is, isNot, spaces, string)
import Data.Foldable ( asum )

type KeyVal = (String, JsonValue)

-- | JSON value representation.
data JsonValue
  = JInteger Int
  | JString String
  | JObject [KeyVal]
  | JArray [JsonValue]
  | JTrue
  | JFalse
  | JNull
  deriving (Show, Eq)

-- | Write a function that applies the given parser, then
--  parse a space character if it exists.
--
-- >>> parse (tok (is 'a')) "a bc"
-- Just ("bc",'a')
--
-- >>> parse (tok (is 'a')) "abc" 
-- Just ("bc",'a')
tok :: Parser a -> Parser a
tok p = p <* spaces

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (isTok 'a') "abc"
-- Just ("bc",'a')
--
-- >>> parse (isTok 'a') "dabc"
-- Nothing
isTok :: Char -> Parser Char
isTok c = is c <* spaces

-- | Write a function that parses a comma followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse commaTok ", bc"
-- Just ("bc",',')
--
-- >>> parse commaTok "dabc"
-- Nothing
commaTok :: Parser Char
commaTok = is ',' <* spaces

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Just ("","abc")
--
-- >>> parse (stringTok "abc") "bc  "
-- Nothing
stringTok :: String -> Parser String
stringTok s = string s <* spaces

-- | Parse a JSON integer.
--
-- >>> parse jsonInteger "234"
-- Just ("",JInteger 234)
--
-- >>> parse jsonInteger "-234"
-- Just ("",JInteger (-234))
--
-- >>> parse jsonInteger "-123.45"
-- Nothing
--
-- >>> parse jsonInteger "-"
-- Nothing
--
-- >>> parse jsonInteger "abc"
-- Nothing
jsonInteger :: Parser JsonValue
jsonInteger = JInteger <$> int

-- | Parse a JSON true literal.
-- /Hint/ Useful function PCQ=
--
-- >>> parse jsonTrue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonTrue "TRUE"
-- Nothing
jsonTrue :: Parser JsonValue
jsonTrue = JTrue <$ string "true"

-- | Parse a JSON false literal.
--
-- >>> parse jsonFalse "false"
-- Just ("",JFalse)
--
-- >>> parse jsonFalse "FALSE"
-- Nothing

jsonFalse :: Parser JsonValue
jsonFalse = JFalse <$ string "false"

-- | Parse a JSON false boolean.
--
-- >>> parse jsonFalse "false"
-- Just ("",JFalse)
--
-- >>> parse jsonTrue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonTrue "TRUE"
-- Nothing
jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse

-- | Parse a JSON null literal.
--
-- >>> parse jsonNull "null"
-- Just ("",JNull)
--
-- >>> parse jsonNull "NULL"
-- Nothing
jsonNull :: Parser JsonValue
jsonNull = JNull <$ string "null"

-- | Parse a sequence of at least one values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) ""
-- Nothing
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a"
-- Just ("","a")
--
-- >>> parse ((isTok 'a') `sepBy1` commaTok) "a,a"
-- Just ("","aa")
--
-- >>> parse ((tok int) `sepBy1` commaTok) "1,2,3"
-- Just ("",[1,2,3])
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p1 p2 = liftA2 (:) p1 (many (p2 *> p1))
  where
    -- (Optional) cons the results of two parsers
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = undefined

-- | Parse a sequence of values with a separator.
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) ""
-- Just ("","")
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) "a"
-- Just ("","a")
--
-- >>> parse ((isTok 'a') `sepBy` commaTok) "a,a"
-- Just ("","aa")
--
-- >>> parse ((tok int) `sepBy` commaTok) "1,2,3"
-- Just ("",[1,2,3])
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = liftA2 (:) p1 (many (p2 *> p1)) <|> pure []

-- | A quoteString is any series of any non-" characters surrounded by " "
--
-- >>> parse quoteString "\" abc\""
-- Just (""," abc")
--
-- >>> parse quoteString "\"abc\"def"
-- Just ("def","abc")
--
-- >>> parse quoteString "abc"
-- Nothing
--
-- >>> parse quoteString "\"\\abc\"def"
-- Just ("def","\\abc")
quoteString :: Parser String
quoteString = is '"' *> many (isNot '"') <* is '"'

-- | Parse a JSON string. Handle double-quotes.
--
-- >>> parse jsonString "\" abc\""
-- Just ("",JString " abc")
--
-- >>> parse jsonString "\"abc\"def"
-- Just ("def",JString "abc")
--
-- >>> parse jsonString "abc"
-- Nothing
--
-- >>> parse jsonString "\"\\abc\"def"
-- Just ("def",JString "\\abc")
jsonString :: Parser JsonValue
jsonString = JString <$> quoteString

-- | Parse a JSON array.
--
-- /Hint/: Remember the type [JsonValue] means a list of JsonValues, and the
--  parser for JsonValue is called jsonValue.
--
-- /Hint 2/: This parser is co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- see https://tgdwyer.github.io/parsercombinators/#creating-a-parse-tree
--
-- >>> parse jsonArray "[]"
-- Just ("",JArray [])
--
-- >>> parse jsonArray "[true]"
-- Just ("",JArray [JTrue])
--
-- >>> parse jsonArray "[true, 5, []]"
-- Just ("",JArray [JTrue,JInteger 5,JArray []])
jsonArray :: Parser JsonValue
jsonArray = (isTok '[') *> (JArray <$> sepBy jsonValue commaTok) <* (isTok ']')

-- | Parse a JSON object.
--
-- /Hint/: Remember the type KeyVal = [(String, JsonValue)].
--
-- /Hint 2/: This parser is also co-recursive!
--  To test this parser, you will need to implement jsonValue as well.
--
-- >>> parse jsonObject "{}"
-- Just ("",JObject [])
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Just ("",JObject [("key1",JTrue)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Just ("",JObject [("key1",JTrue),("key2",JFalse)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Just ("xyz",JObject [("key1",JTrue),("key2",JFalse)])
jsonObject :: Parser JsonValue
jsonObject = isTok '{' *> (JObject <$> sepBy kv commaTok) <* isTok '}'
  where
    kv :: Parser (String, JsonValue)
    kv = (,) <$> quoteString <*> (spaces *> isTok ':' *> jsonValue <* spaces)

-- | Parse a JSON value.
--
-- /Hint/: Use the parsers we have just made for each component of the JSON
--  type.
--
--
-- >>> parse jsonValue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse])])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse]),("key3",JObject [("key4",JNull)])])
jsonValue :: Parser JsonValue
jsonValue = jsonInteger <|> jsonBool <|> jsonNull <|> jsonString <|> jsonArray <|> jsonObject
