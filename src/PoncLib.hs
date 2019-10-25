module PoncLib
  ( commandParse ) where

import           Parse              (filePath2Stack)
import           Shared
import           Typing

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Data.Void          (Void)
import           Safe               (headMay)
import           System.Environment (getArgs)
import           Text.Megaparsec    (ParseErrorBundle, errorBundlePretty)
import           Text.Pretty.Simple as PrettyS

type Source = Text

commandParse :: [FilePath] -> IO ()
commandParse filePaths =
  fmap parseSource ((\path -> (,) path <$> TIO.readFile path) `mapM` filePaths)
  >>= mapM_ (either (putStrLn . errorBundlePretty) PrettyS.pPrint)

parseSource :: [(FilePath, Source)] -> [Either ErrorBundle Stack]
parseSource srcs = uncurry filePath2Stack <$> srcs


-- commandType :: [String] -> IO ()
-- commandType files = commandParse files
--                     >>= mapM_ (either (putStrLn . errorBundlePretty)
--                                 (PrettyS.pPrint . stack2typedStack))

typeSource ::  [(FilePath, Source)] -> [Either ErrorBundle TypedStack]
typeSource pathSrcs = undefined -- fmap stack2typedStack <$> parseSource pathSrcs

