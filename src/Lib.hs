module Lib where

import           Control.Monad.State  (State, evalState, get, gets, lift,
                                       modify, put)
import           Data.Char            (isAscii)
import           Data.Map             (Map, elems, empty, insert, (!?))
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Data.Void            (Void)
import           Safe                 (maximumDef)
import           Text.Megaparsec      (ParseErrorBundle, ParsecT, between,
                                       choice, eof, errorBundlePretty, many,
                                       runParserT, single, takeWhile1P, (<|>))
import           Text.Megaparsec.Char (space)
-- import           Text.Megaparsec.Debug (dbg)
import           Text.Pretty.Simple   as PrettyS

data Expr = DefLambda { body :: Expr }
          | ExprVal   { index :: Integer }
          | Apply     { exprFunc :: Expr, exprArg :: Expr }
          deriving (Show, Eq)

-- newtype Val = Val Integer deriving (Show, Ord, Eq)

type Parser = ParsecT Void Text (State (Map Text Integer))


ident :: Parser Text
ident = takeWhile1P (Just "identifier") (`elem` ['a'..'z'] <> ['A'..'Z'])


execParseFilePrint :: [String] -> IO ()
execParseFilePrint files =
  execParseFile files
  >>= mapM_ (either (putStrLn . errorBundlePretty) PrettyS.pPrint)


execParseFile :: [String] -> IO [Either (ParseErrorBundle Text Void) Expr]
execParseFile files =
  (\file -> src2Expr file <$> TIO.readFile file)
  `mapM` files


src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Expr
src2Expr fPath src = evalState (runParserT (pExpr <* space <* eof) fPath src) empty


pVal :: Parser Integer
pVal = --dbg "val" $
  do
  var <- ident
  env <- get
  maybe (fail "unbound var") pure $ env !? var


pExpr :: Parser Expr
pExpr = --dbg "expr" $
  do
  fstArg    <- pTerm
  argAndOps <- many $ flip Apply <$> pTerm
  pure $ foldr (flip (.)) id argAndOps fstArg


pLamdba :: Parser Expr
pLamdba = --dbg "lambda" $
  do
  space
  _     <- single '\\' <|> single '^'
  bound <- ident <* single '.'
  depth <- lift $ gets $ maximumDef (-1) . elems
  lift $ modify $ insert bound (succ depth)
  body  <- pExpr
  pure $ DefLambda { body = body }


pTerm :: Parser Expr
pTerm = --dbg "term" $
  between space space $ choice
        [ pLamdba
        , ExprVal <$> pVal
        , single '(' *> pExpr <* space <* single ')'
        ]
