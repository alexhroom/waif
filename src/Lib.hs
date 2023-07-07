module Lib (
    parseDT,
    invertPairs,
    Event
    ) where

import Data.Time
import Data.Time.Format.ISO8601

type Event = (UTCTime, UTCTime)

{-
Inverts a list of pairs, discarding the first value of the first tuple
and the last value of the last tuple.

Example: 
invertPairs [(1, 2) (3, 4) (5, 6)] = [(2, 3) (4, 5)]
-}
invertPairs :: [(a, a)] -> [(a, a)]
invertPairs (x:y:zs) = (snd x, fst y) : invertPairs (y:zs)
invertPairs _ = []

{-
Parses iCalendar datetime to UTC time.

Example:
parseDT "19970714T170000Z" = 1997-07-14 17:00:00 UTC
-}
parseDT :: String -> UTCTime
parseDT = parseTimeOrError True defaultTimeLocale "%Y%m%dT%H%M%S%Z"

--normaliseDT :: String -> String
