{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( 
    downloadProblem, downloadTodayProblem, setup, saveSession, submitAnswer, submitTodayAnswer) where

import Network.HTTP.Simple
    ( getResponseBody,
      getResponseStatus,
      httpLBS )
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types.Status as HTTPStatus
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory )
import System.FilePath ( (</>) )
import System.Environment ()
import Data.Time.Clock.POSIX ()
import Data.Time.Clock
import Data.Time.Format
import Data.Time
import Text.Regex.TDFA
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (unless)

aocUrl :: String
aocUrl = "https://adventofcode.com"

getDataPath :: IO FilePath
getDataPath = (</> "data") <$> getCurrentDirectory

getSessionPath :: IO FilePath
getSessionPath = (</> "session") <$> getDataPath

getAnswerTemplatePath :: IO FilePath
getAnswerTemplatePath = (</> "templates/AnswerTemplate.hs") <$> getCurrentDirectory

setup :: IO()
setup = do
    dataPath <- getDataPath
    sessionPath <- getSessionPath
    createDirectoryIfMissing True dataPath
    putStrLn $ "Data folder created (or already exists): " ++ dataPath
    doesSessionExists <- doesFileExist sessionPath
    unless doesSessionExists $ writeFile sessionPath ""

addSessionCookie :: Request -> String -> Request
addSessionCookie req cookieValue =
    req {
        requestHeaders = requestHeaders req ++ [("Cookie", BS.pack $ "session=" ++ cookieValue)]
        }

downloadFile :: (String, FilePath, String) -> String -> IO ()
downloadFile (name, filePath, url) sessionValue = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            putStrLn $ "Found cached " ++ url ++ " at " ++ filePath
    else do
        request <- parseRequest url
        let requestWithCookie = addSessionCookie request sessionValue
        response <- httpLBS requestWithCookie
        if getResponseStatus response == HTTPStatus.status200
            then do
                LBS.writeFile filePath (getResponseBody response)
                putStrLn $ "Downloaded " ++ url ++ " to " ++ filePath
            else
                putStrLn $ "Failed to download " ++ name ++ " from " ++ url ++ ". Status code: " ++ show (getResponseStatus response)

downloadProblem :: String -> IO ()
downloadProblem day = do
    let dayInt = read day :: Int
    if dayInt > 0 && dayInt <= 25 then do
        year <- getCurrentYearEST
        sessionMaybe <- getSession
        case sessionMaybe of
            Left sessionError -> putStrLn sessionError
            Right session -> do
                dataPath <- getDataPath
                let dayUrl = getDayUrl year day
                    inputUrl = dayUrl ++ "/input"
                    dayPath = dataPath </> day
                    descPath = dayPath </> day ++ ".html"
                    inputPath = dayPath </> day ++ ".input"
                    answerPath = dayPath </> day ++ ".answer.hs"
                    urlPaths = [("description", descPath, dayUrl), ("input", inputPath, inputUrl)]
                putStrLn ""
                putStrLn $ "Day " ++ day
                createDirectoryIfMissing True dayPath
                putStrLn $ "Day folder created (or already exists): " ++ dayPath
                mapM_ (`downloadFile` session) urlPaths
                doesFileExist answerPath >>= (\answerPathExists -> do
                    answerTemplatePath <- getAnswerTemplatePath
                    answerTemplateContent <- TIO.readFile answerTemplatePath
                    let answerContent = T.replace "{day}" (T.pack day) answerTemplateContent
                    unless answerPathExists (TIO.writeFile answerPath answerContent >> putStrLn ("Answer template copied to " ++ answerPath)))
            else do
                putStrLn "Only valid dates are 1st to Christmas."

downloadTodayProblem :: IO ()
downloadTodayProblem = getCurrentDateEST >>= downloadProblem

getDayUrl :: [Char] -> [Char] -> [Char]
getDayUrl year day = aocUrl ++ "/" ++ year ++ "/day/" ++ day

-- puzzles are released at midnight EST
getCurrentTimeESTWithFormat :: String -> IO String
getCurrentTimeESTWithFormat format = do
  currentTime <- getCurrentTime
  let estTimeZone = hoursToTimeZone (-5)
      estTime = utcToLocalTime estTimeZone currentTime
      formattedESTDate = formatTime defaultTimeLocale format estTime
  return formattedESTDate

getCurrentDateEST :: IO String
getCurrentDateEST = getCurrentTimeESTWithFormat "%-d"

getCurrentYearEST :: IO String
getCurrentYearEST = getCurrentTimeESTWithFormat "%Y"

getSession :: IO (Either String String)
getSession = do
    sessionPath <- getSessionPath
    session <- readFile sessionPath
    if session == "" then
        return $ Left "Session is empty. Please use --help to see how to set up session first."
    else
        return $ Right session

saveSession :: String -> IO ()
saveSession val = do
    sessionPath <- getSessionPath
    writeFile sessionPath val
    putStrLn $ "Saved session in " ++ sessionPath

submitTodayAnswer :: [Char] -> IO ()
submitTodayAnswer = (getCurrentDateEST >>=) . flip submitAnswer

submitAnswer :: String -> String -> IO ()
submitAnswer day answer = do
    year <- getCurrentYearEST
    dataPath <- getDataPath
    sessionMaybe <- getSession
    case sessionMaybe of
        Left sessionError -> putStrLn sessionError
        Right session -> do
            let dayUrl = getDayUrl year day ++ "/answer"
                ansPath = dataPath </> day </> day ++ ".answer.html"
                formData = "level=" ++ day ++ "&answer=" ++ answer
                headers = [("Content-Type", "application/x-www-form-urlencoded")]
            request <- parseRequest dayUrl
            let postRequest = request
                    { method = "POST"
                    , requestHeaders = headers
                    , requestBody = RequestBodyBS $ BS.pack formData
                    }
                requestWithCookie = addSessionCookie postRequest session
            response <- httpLBS requestWithCookie
            if getResponseStatus response == HTTPStatus.status200
                then do
                    let body = getResponseBody response
                    LBS.writeFile ansPath body
                    putStrLn $ "Saved answer response of day " ++ day ++ ": " ++ answer ++ " to " ++ ansPath
                    let (_, _, _, result) = body =~ LBS.pack "<article>(.*)</article>" :: (LBS.ByteString, LBS.ByteString, LBS.ByteString, [LBS.ByteString])
                    putStrLn ""
                    LBS.putStrLn $ head result
                else
                    putStrLn $ "Failed to submit ans for day " ++ day ++ ". Status code: " ++ show (getResponseStatus response)