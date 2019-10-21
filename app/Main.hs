module Main where

import           CullyLib

import           Safe               (headMay)
import           System.Environment (getArgs)

main :: IO ()
main = do
  ops <- getArgs
  maybe help (\com -> case com of
                 "parse" -> execParseFilePrint $ tail ops
                 _       -> help)
    $ headMay ops

help :: IO ()
help = putStrLn "COMMANDS: parse"
