module Main (main) where

import System.Environment (getArgs)

import Lib (downloadTodayProblem, downloadProblem, saveSession, setup, submitAnswer)

main :: IO ()
main = do
    setup
    args <- getArgs
    case args of
        ["session", val] -> saveSession val
        ["dl"] -> downloadTodayProblem
        ["dl", day]       -> downloadProblem day
        ["ans", day, answer] -> submitAnswer day answer
        ["--help"] -> printHelp
        _                  -> printHelp

printHelp :: IO ()
printHelp = putStrLn "\nUsage: ./aoc [dl [day] | session <cookieValue> | ans <day> <answer> | --help] \n \
    \ 1. For first time setup, you need to log into AOC in the browser. \n \
    \ 2. See https://developer.chrome.com/docs/devtools/application/cookies to get the session cookie value. \n \
    \ 3. Set the session cookie for the runner by ./aoc session <cookieValue>"