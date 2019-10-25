module Main where

import           PoncLib

import           Safe               (headMay)
import           System.Environment (getArgs)
import           Text.Megaparsec    (errorBundlePretty)
import           Text.Pretty.Simple as PrettyS

main :: IO ()
main = do
  ops <- getArgs
  maybe help (\com -> case com of
                 "parse" -> commandParse $ tail ops
                 "type"  -> execParseFile (tail ops)
                            >>= mapM_ (either (putStrLn . errorBundlePretty)
                                        (PrettyS.pPrint . stack2typedStack))
                 _       -> help)
    $ headMay ops

help :: IO ()
help = putStrLn "COMMANDS: parse, type"
