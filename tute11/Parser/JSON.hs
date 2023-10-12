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
module JSON (module JSON) where

import Control.Applicative (Alternative (many, (<|>)), Applicative (liftA2))
import Data.Foldable (asum)
import Instances (Parser (..), int)
import Parser (is, isNot, spaces, string)

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

-- | Write a function that applies the given parser, the
--  parse an a space character if it exists.
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
isTok = tok . is

commaTok :: Parser Char
commaTok = isTok ','

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
stringTok = tok . string

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
jsonInteger = JInteger <$> tok int

-- | Parse a JSON true literal.
--
-- >>> parse jsonTrue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonTrue "TRUE"
-- Nothing
jsonTrue :: Parser JsonValue
jsonTrue = JTrue <$ stringTok "true"

-- | Parse a JSON false literal.
--
-- >>> parse jsonFalse "false"
-- Just ("",JFalse)
--
-- >>> parse jsonFalse "FALSE"
-- Nothing
jsonFalse :: Parser JsonValue
jsonFalse = JFalse <$ stringTok "false"

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
jsonNull = JNull <$ stringTok "null"

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
sepBy1 p sep = p <:> many (sep *> p)
  where
    -- (Optional) Join two parsers into a list
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = liftA2 (:)

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
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse a JSON string. Handle double-quotes.
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
quoteString = is '"' *> many (tok $ isNot '"') <* isTok '"'

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
jsonArray = isTok '[' *> (JArray <$> (jsonValue `sepBy` commaTok)) <* isTok ']'

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
jsonObject = isTok '{' *> (JObject <$> (jsonKeyVal `sepBy` commaTok)) <* isTok '}'
  where
    -- keyval s v = liftA2 (,) (s <* isTok ':') v
    keyval = liftA2 (,) . (<* isTok ':')
    jsonKeyVal = keyval quoteString jsonValue

-- | Parse a JSON value.
--
-- /Hint/: Use the parsers we have just made for each component of the JSON
--  type.
--
-- /Hint 2/: The parsers we have created only parse the values, but we need
--  to wrap them or convert them to the JsonValue type. What typeclass have
--  we seen that can do this? What are some useful operators?
--
-- >>> parse jsonValue "true"
-- Just ("",JTrue)
--
-- >>> parse jsonValue "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse])])
--
-- >>> parse jsonValue "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Just ("",JObject [("key1",JTrue),("key2",JArray [JInteger 7,JFalse]),("key3",JObject [("key4",JNull)])])
jsonValue :: Parser JsonValue
-- jsonValue = tok $ foldr1 (<|>) parsers
jsonValue = tok $ asum parsers
  where
    parsers =
      [ jsonInteger,
        jsonNull,
        jsonTrue,
        jsonFalse,
        jsonArray,
        jsonObject,
        jsonString
      ]
