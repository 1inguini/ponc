{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( execParseFile
  , execParseFilePrint) where

import           Control.Monad.State.Strict (State, get, gets, lift, modify,
                                             put, runState)
import           Data.Char                  (isAscii)
import           Data.Map                   (Map, elems, empty, insert, (!?))
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void                  (Void)
import           Safe                       (maximumDef)
import           Text.Megaparsec            (ParseErrorBundle, Parsec, ParsecT,
                                             SourcePos, Stream, between, choice,
                                             errorBundlePretty, getSourcePos,
                                             initialPos, many, parse,
                                             runParserT, sepBy, takeWhile1P,
                                             try, (<|>))
import           Text.Megaparsec            as P (eof, tokens)
import           Text.Megaparsec.Char       as P (space)
import           Text.Megaparsec.Char.Lexer as P (decimal)
-- import           Text.Megaparsec.Debug (dbg)
import           Text.Pretty.Simple         as PrettyS

execParseFilePrint :: [String] -> IO ()
execParseFilePrint files =
  execParseFile files
  >>= mapM_ (either (putStrLn . errorBundlePretty) PrettyS.pPrint)


execParseFile :: [String] -> IO [Either (ParseErrorBundle Text Void) Stack]
execParseFile files =
  (\file -> src2Expr file <$> TIO.readFile file)
  `mapM` files

type Parser = Parsec Void Text

newtype Stack = Stack { unStack ::  [(SourcePos, Stackable)] }
              deriving (Eq)

instance Show Stack where
  show (Stack stack) = show $ snd <$> stack

data Type = Nat
          | Func { args :: [Type], result :: Type }
          deriving (Show, Eq)

data Stackable = DefLambda { lambdaType :: Type, body :: Stack }
               | StackedVal Val
               | RecStack Stack
               deriving (Show, Eq)

data Val = IntVal Integer
         | Zero
         | Bounded Integer
         | SuccNotPred Bool
         deriving (Show, Ord, Eq)


wrap :: Parser a -> Parser (SourcePos, a)
wrap p = (,) <$> getSourcePos <*> p


parens :: Parser a -> Parser a
parens = between ("(" *> space) (space <* ")")

ident :: Parser Text
ident = takeWhile1P (Just "identifier") (`elem` -- ['a'..'z'] <>
                                         ['A'..'Z'] -- <> ['0'..'9'] <> "!$%&*+/<=>?@\\^|-~:#"
                                        )

succSym, predSym :: IsString a => a
succSym = "succ"
predSym = "pred"

pVal :: Parser Val
pVal = --dbg "val" $
  choice
  [ Bounded <$> ("$" *> decimal)
  , IntVal <$> decimal
  , Zero <$ "0"
  , SuccNotPred True <$ succSym
  , SuccNotPred False <$ predSym
  ]


-- src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Stackable]
-- src2Expr filePath src = evalState (runParserT (pExpr <* space <* eof) filePath src) $ initialPos filePath

-- src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Stackable]
-- src2Expr filePath src = snd <$> parse (unParser (pExpr <* space <* eof)) filePath src

src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Stack
src2Expr = parse (pExpr <* space <* eof)


pExpr :: Parser Stack
pExpr = --dbg "expr" $
  Stack <$> (space *> many (wrap pTerm))

pLambda :: Parser Stackable
pLambda = --dbg "lambda" $
  do
  argTys <- space *> "*" *> (pType `sepBy` (space *> ",")) <* space <* ";"
  body  <- pExpr
  pure $ DefLambda { lambdaType = Func { args = argTys, result = Nat }
                   , body = body }
    where
      pType = do
        result <- space *> pTypeTerm
        args   <- many $ space *> "^" *> space *> pTypeTerm
        pure $ case args of
          [] -> result
          _  -> Func { args = args, result = result }


      pTypeTerm = parens pType <|> Nat <$ "N"

pTerm :: Parser Stackable
pTerm = --dbg "term" $
  between space space $ choice
  [ pLambda
  , StackedVal <$> pVal
  , RecStack <$> parens pExpr
  ]
