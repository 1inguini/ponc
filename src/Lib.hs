module Lib where

import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Data.Void            (Void)
import           Text.Megaparsec      (ParseErrorBundle, Parsec, between,
                                       choice, eof, errorBundlePretty, many,
                                       parse, single, (<|>))
import           Text.Megaparsec.Char (alphaNumChar, space)
import           Text.Pretty.Simple   as PrettyS


data Expr = DefLambda { freeVar :: Val, body :: Expr }
          | ExprVal { exprVal :: Val }
          | Apply { exprFunc :: Expr, exprArg :: Expr }
          deriving (Show, Eq)

newtype Val = Val Char deriving (Show, Eq)

type Parser = Parsec Void Text

execParseFilePrint :: [String] -> IO ()
execParseFilePrint files =
  execParseFile files
  >>= mapM_ (either (putStrLn . errorBundlePretty) PrettyS.pPrint)


execParseFile :: [String] -> IO [Either (ParseErrorBundle Text Void) Expr]
execParseFile files =
  (\file -> src2Expr file <$> TIO.readFile file)
  `mapM` files


src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Expr
src2Expr = parse (pExpr <* space <* eof)


pVal :: Parser Val
pVal = Val <$> alphaNumChar

pExpr :: Parser Expr
pExpr = do
  fstArg    <- pTerm
  argAndOps <- many $ flip Apply <$> pTerm
  pure $ foldr (flip (.)) id argAndOps fstArg

pLamdba :: Parser Expr
pLamdba = do
  space
  _    <- single '\\' <|> single '^'
  free <- pVal
  _    <- single '.'
  body <- pExpr
  pure $ DefLambda { freeVar = free, body = body }

pTerm :: Parser Expr
pTerm = between space space $ choice
        [ pLamdba
        , ExprVal <$> pVal
        , single '(' *> pExpr <* space <* single ')'
        ]
