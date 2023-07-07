module Main (main) where

import Data.Time
import System.IO
import System.Directory

import Lib
import Parse

main :: IO ()
main = putStrLn "Hello World!"

--Get all calendar events between two days.
dayRange :: Day -> Day -> [Event] -> [Event]
dayRange start end dates = filter (\x -> ((utctDay $ fst x) < start) || ((utctDay $ snd x) > end)) dates

processCalendar :: FilePath -> IO [Event]
processCalendar file = do
    contents <- readFile file
    let calendar = parseCalendar contents
    return calendar