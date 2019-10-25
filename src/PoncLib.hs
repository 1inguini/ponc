module PoncLib
  ( commandParse
  , commandType
  ) where

import           Shared

import           Parse              (filePathSrc2Stack)
import           Typing             (stack2typedStack)

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Text.Megaparsec    (errorBundlePretty)
import           Text.Pretty.Simple as PrettyS

type Source = Text

commandParse :: [FilePath] -> IO ()
commandParse = commandPrint (fromFile parseSource)

commandType :: [FilePath] -> IO ()
commandType = commandPrint (fromFile typeSource)

commandPrint :: Show a => (FilePath -> IO (Either ErrorBundle a)) -> [FilePath] -> IO ()
commandPrint command filePaths =
  command `mapM` filePaths
  >>= mapM_ (either (putStrLn . errorBundlePretty) PrettyS.pPrint)

fromFile :: ((FilePath, Source) -> Either ErrorBundle a)
         -> FilePath
         -> IO (Either ErrorBundle a)
fromFile interpret filePath = interpret <$> ((,) filePath <$> TIO.readFile filePath)

parseSource :: (FilePath, Source) -> Either ErrorBundle Stack
parseSource = uncurry filePathSrc2Stack

typeSource :: (FilePath, Source) -> Either ErrorBundle TypedStack
typeSource pathSrcs = parseSource pathSrcs >>= stack2typedStack

