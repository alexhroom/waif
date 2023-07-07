{-# LANGUAGE ApplicativeDo #-}
module Parse (parseCalendar) where

import Text.Regex.Applicative
import Data.Time

import Lib (parseDT, Event)

parseEvent :: RE String Event
parseEvent = do
    _ <- few anySym
    _ <- string "DTSTART:"
    start <- few anySym
    _ <- char '\n'
    _ <- few anySym
    _ <- string "DTEND:"
    end <- few anySym
    _ <- char '\n'
    _ <- many anySym

    return (parseDT start, parseDT end)

parseCalendar :: String -> [Event]
parseCalendar = handleError . parse (many parseEvent <|> many1 anyChar *> void) ""
  where
    handleError :: Either ParseError [Event] -> [Event]
    handleError (Right msg) = msg
    handleError (Left _) = []

--parseEvent :: Parsec String st Event
--parseEvent = do
--    _ <- manyTill anyChar (string "DTSTART:")
--    start <- manyTill anyChar (string "\n")
--    _ <- manyTill anyChar (string "DTEND:")
--    end <- manyTill anyChar (string "\n")
--    return (parseDT start, parseDT end)
