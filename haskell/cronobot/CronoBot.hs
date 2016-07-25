{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           BasePrelude
import           Data.Aeson (Value)
import           Data.Aeson.Types (Object, Result(..), parse, (.:))
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack)
import           Data.Time
import           Network.HTTP.Simple
import           Network.Linklater
import           Network.URI (escapeURIString, isUnescapedInURIComponent)
import           Network.Wai.Handler.Warp (run)


port = 9000
gBaseUrl = "https://sheets.googleapis.com/v4/spreadsheets/"
gSheetID = "1Y9hRtRkfHwriZBIheQltTUfbz6YoSZ7mzNgCa-MiB-I"
gKey = "AIzaSyAdxRGGvBiqpB5m0QPd4P31b6ibQI8WMnA"
dayToSheet = Map.fromList ([ (fromGregorian 2016 07 18, "lun 18 / 7")
                           , (fromGregorian 2016 07 19, "mar 19 / 7")
                           , (fromGregorian 2016 07 20, "mer 20 / 7")
                           , (fromGregorian 2016 07 21, "gio 21 / 7")
                           , (fromGregorian 2016 07 22, "ven 22 / 7")
                           , (fromGregorian 2016 07 23, "sab 23 / 7")
                           , (fromGregorian 2016 07 24, "dom 24 / 7")
                           , (fromGregorian 2016 07 25, "lun 25 / 7")
                           , (fromGregorian 2016 07 26, "mar 26 / 7")
                           , (fromGregorian 2016 07 27, "mer 27 / 7")
                           , (fromGregorian 2016 07 28, "gio 28 / 7")
                           , (fromGregorian 2016 07 29, "ven 29 / 7")
                           , (fromGregorian 2016 07 30, "sab 30 / 7") ])


getLocations :: [[String]] -> [String]
getLocations values = tail $ values !! 0


parseSheet :: Object -> Result [[String]]
parseSheet v =
  parse (\obj -> do
    values <- obj .: "values"
    return (values))
  v


hourMinToIndex :: Int -> Int -> Int
hourMinToIndex h m
  | (h == 9 && m >= 30) || (h == 10 && m <= 30) = 1
  | (h == 10 && m >= 30) || (h == 11 && m <= 30) = 2
  | (h == 11 && m >= 30) || (h == 12 && m <= 30) = 3
  | (h == 12 && m >= 30) || (h == 13 && m <= 30) = 4
  | (h == 13 && m >= 30) || (h == 14 && m <= 30) = 5
  | (h == 14 && m >= 30) || (h == 15 && m <= 30) = 6
  | (h == 15 && m >= 30) || (h == 16 && m <= 30) = 7
  | (h == 16 && m >= 30) || (h == 17 && m <= 30) = 8
  | (h == 17 && m >= 30) || (h == 18 && m <= 30) = 9
  | (h == 18 && m >= 30) || (h == 19 && m <= 30) = 10
  | (h == 19 && m >= 30) || (h == 20 && m <= 30) = 11
  | (h == 20 && m >= 30) || (h == 21 && m <= 30) = 12
  | (h == 21 && m >= 30) || (h == 22 && m <= 30) = 13
  | (h == 22 && m >= 30) || (h == 23 && m <= 30) = 14
  | otherwise = 0


getTime :: IO TimeOfDay
getTime = do
  time <- getZonedTime
  return (localTimeOfDay $ zonedTimeToLocalTime time)


getTimeSlot :: IO Int
getTimeSlot = do
  tod <- getTime
  return (hourMinToIndex (todHour tod) (todMin tod))


getSheet :: IO String
getSheet = do
  date <- getCurrentTime
  return ((case (Map.lookup (utctDay date) dayToSheet) of
    Just s -> s
    Nothing -> "lun 18 / 7") ++ "!A1:H16")


buildGSURL :: String -> String -> String -> String -> String
buildGSURL baseUrl sheetID key range =
  baseUrl ++ sheetID ++ "/values/" ++ range ++ "?key=" ++ key


reply :: Maybe Command -> IO Text
reply (Just command) = do
  putStrLn $ " + Incoming command: " ++ show command
  sheet <- getSheet
  -- let sheet = "ven 22 / 7!A1:H16"
  request <- parseRequest (url $ escapeURIString isUnescapedInURIComponent sheet)
  response <- httpJSON request
  let respBody = getResponseBody response :: Object
  case (parseSheet respBody) of
    Success parsedSheet -> do
      let locations = getLocations parsedSheet
      timeSlot <- getTimeSlot
      -- let timeSlot = 3
      if timeSlot == 0
      then do
        return ("No activities right now")
      else do
        let activities = parsedSheet !! timeSlot
        if length activities <= 2
        then return ("No activities right now")
        else do
          let actLocs = map (\(l, a) -> l ++ ": " ++ a) (zip locations (tail activities))
          let botResponse = intercalate "\n\n" actLocs
          putStrLn $ " + Responding with:\n" ++ botResponse ++ "\n\n"
          return $ pack $ botResponse
    Error err -> do
      putStrLn $ " + Got error parsing JSON object: " ++ err
      return ""
  where
    url = buildGSURL gBaseUrl gSheetID gKey
reply Nothing = do return ""


main :: IO ()
main = do
  putStrLn (" + Listening on port " ++ (show port))
  run port (slashSimple reply)
