module Main where

import Control.Applicative
import Data.List (sort)
import System.Process (readProcess)

import System.Environment (getArgs)

import System.Locale (defaultTimeLocale)
import System.Time (ClockTime(TOD), diffClockTimes, formatTimeDiff, normalizeTimeDiff)
import Data.Time.Format (parseTime, formatTime, FormatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime, localTimeToUTC)
import Data.Time.Clock (DiffTime, UTCTime, UTCTime(..), getCurrentTime, addUTCTime)

readLast :: String -> IO [String]
readLast username = do
  raw <- readProcess "last" ["-F", username] ""
  return $ lines raw

extractDateString :: String -> String
extractDateString = unwords . take 4 . drop 4 . words

extractDateStrings :: [String] -> [String]
extractDateStrings = map extractDateString

parseDateString :: TimeZone -> String -> Maybe UTCTime
parseDateString tz str = localTimeToUTC tz <$> parseTime defaultTimeLocale "%b %e %X %Y" str

parseDateStrings :: TimeZone -> [String] -> [Maybe UTCTime]
parseDateStrings tz = map $ parseDateString tz

filterToday :: UTCTime -> UTCTime -> Bool
filterToday today time = utctDay today == utctDay time

liftMaybeBool :: Maybe Bool -> Bool
liftMaybeBool Nothing = False
liftMaybeBool (Just x) = x

filterMaybe :: UTCTime -> [Maybe UTCTime] -> [Maybe UTCTime]
filterMaybe today = filter (liftMaybeBool . fmap (filterToday today))

add8Hours :: UTCTime -> UTCTime
add8Hours = addUTCTime (8 * 60 * 60)

formatHour :: FormatTime t => t -> String
formatHour = formatTime defaultTimeLocale "%R"

toDiffTime :: Real t => t -> DiffTime
toDiffTime = fromRational . toRational

formatAsTOD :: FormatTime t => t -> Integer
formatAsTOD = read . formatTime defaultTimeLocale "%s"

toClockTime :: FormatTime t => t -> ClockTime
toClockTime t = (\s -> TOD s 0) $ formatAsTOD t

liftMaybeString :: Maybe String -> String
liftMaybeString Nothing = "00:00"
liftMaybeString (Just s) = s

padWithSpaces :: String -> String
padWithSpaces s = " " ++ s ++ " "

padWithParens :: String -> String
padWithParens s = "(" ++ s ++ ")"

remainingTime :: Maybe UTCTime -> ClockTime -> String
remainingTime endUTC now = liftMaybeString $ pad <*> formattedString
  where
    endClockTime = toClockTime <$> endUTC
    difference = fmap (\end -> if now > end
                      then diffClockTimes now end
                      else diffClockTimes end now) endClockTime
    formattedString = formatTimeDiff defaultTimeLocale "%R" <$> normalizeTimeDiff <$> difference
    pad = fmap (\end -> if now > end then padWithParens else padWithSpaces) endClockTime

main :: IO ()
main = do
  username <- getArgs
  lastLogins <- readLast $ head username
  localTimeZone <- getCurrentTimeZone

  utcNow <- getCurrentTime

  let now = toClockTime utcNow
  let nowStr = padWithSpaces $ formatHour $ utcToLocalTime localTimeZone utcNow

  let parse = parseDateStrings localTimeZone . extractDateStrings

  let firstLoginUTC = head $ sort <$> filterMaybe utcNow $ parse $ lastLogins
  let firstLogin = toClockTime <$> firstLoginUTC
  let firstLoginStr = padWithSpaces
                    $ liftMaybeString
                    $ formatHour
                   <$> utcToLocalTime localTimeZone
                   <$> firstLoginUTC

  let endUTC = add8Hours <$> firstLoginUTC
  let endStr = padWithSpaces
             $ liftMaybeString
             $ formatHour
            <$> utcToLocalTime localTimeZone
            <$> endUTC

  let remainingTimeStr = remainingTime endUTC now

  let elapsedTimeStr = padWithSpaces
                     $ liftMaybeString
                     $ formatTimeDiff defaultTimeLocale "%R"
                    <$> normalizeTimeDiff
                    <$> diffClockTimes now
                    <$> firstLogin

  let strings = [firstLoginStr, elapsedTimeStr, nowStr, remainingTimeStr, endStr]

  putStrLn " start | elaps | now   | rem   | end"
  putStrLn $ foldl (\s1 s2 -> s1 ++ "|" ++ s2) (head strings) (tail strings)
