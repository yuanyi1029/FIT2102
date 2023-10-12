{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Instances
import JSON
import Web.Scotty
import Wordle

-- import Data.Text.Encoding (decodeUtf8)

getTimeSeed :: IO Int
getTimeSeed = do
  -- Get the current time
  currentTime <- getCurrentTime
  -- Convert the current time to an integer (number of picoseconds since the epoch)
  let seed = fromEnum (utcTimeToPOSIXSeconds currentTime)
  return seed

getResult :: Maybe (String, JsonValue) -> Maybe JsonValue
getResult (Just (_, jv)) = Just jv
getResult _ = Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

getKey :: String -> JsonValue -> Maybe JsonValue
getKey k (JObject kv) = snd <$> safeHead (filter ((== k) . fst) kv)
getKey _ _ = Nothing

getString :: Maybe JsonValue -> String
getString (Just (JString s)) = s
getString _ = "error"

stringToMaybe :: String -> Maybe String
stringToMaybe "" = Nothing
stringToMaybe x = Just x

main :: IO ()
main = scotty 3000 $ do
  post "/api/makeGuess" $ do
    requestBody <- body
    -- Convert the raw request body to a Text
    let requestBodyText = decodeUtf8 requestBody

    -- Parse the JSON manually (replace this with your actual parsing logic)
        parsedJson = parse jsonValue (unpack requestBodyText)
        guess = getString (getKey "guess" =<< getResult parsedJson)
        target = getString (getKey "target" =<< getResult parsedJson)
        previousGuess = stringToMaybe $ getString (getKey "previousGuess" =<< getResult parsedJson)

    valid <- liftIO $ (ensureCriteria previousGuess guess target &&) <$> checkValid guess
    json $
      object
        [ "feedback" .= (pack (show $ show <$> makeFeedback guess target) :: Text),
          "valid" .= pack (show valid)
        ]

  post "/api/getTarget" $ do
    (_, randAnswer) <- liftIO $ generateRandomAnswer =<< getTimeSeed
    json $ object ["target" .= randAnswer]

  get "/" $ do
    html "<h1>This is the endpoint for the server. Go to /javascript and run <code>npm install</code> and <code>npm run dev</code> to start the web app front-end.</h1>"
