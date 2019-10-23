{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( execParseFile
  , execParseFilePrint
  , Stack ) where

import           Shared                     (Node (..), Stack (..), Type (..),
                                             Val (..))

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
                                             SourcePos, Stream, Tokens, between,
                                             choice, errorBundlePretty,
                                             getSourcePos, initialPos, many,
                                             parse, runParserT, sepBy1,
                                             sepEndBy, takeWhile1P, try, (<|>))
import           Text.Megaparsec            as P (eof, tokens)
import qualified Text.Megaparsec.Char       as C (space1)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme,
                                                  skipBlockCommentNested,
                                                  skipLineComment, space)
import           Text.Megaparsec.Debug      (dbg)
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

wrap :: Parser a -> Parser (SourcePos, a)
wrap p = (,) <$> getSourcePos <*> p

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "ln") (L.skipBlockCommentNested "blkC" "endC")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parens, braces, angles, brackets :: Parser a -> Parser a

parens   = between ("(" *> space) ")"
braces   = between ("{" *> space) "}"
angles   = between ("<" *> space) ">"
brackets = between ("[" *> space) "]"

-- ident :: Parser Text
-- ident = takeWhile1P (Just "identifier") (`elem` -- ['a'..'z'] <>
--                                          ['A'..'Z'] -- <> ['0'..'9'] <> "!$%&*+/<=>?@\\^|-~:#"
--                                         )

succSym, predSym :: IsString a => a
succSym = "succ"
predSym = "pred"

pVal :: Parser Val
pVal = --dbg "val" $
  lexeme $ choice
  [ Bounded <$> ("$" *> decimal)
  -- , IntVal <$> decimal
  , Zero <$ "0"
  , SuccNotPred True <$ succSym
  , SuccNotPred False <$ predSym
  ]


-- src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Node]
-- src2Expr filePath src = evalState (runParserT (pExpr <* space <* eof) filePath src) $ initialPos filePath

-- src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Node]
-- src2Expr filePath src = snd <$> parse (unParser (pExpr <* space <* eof)) filePath src

src2Expr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Stack
src2Expr = parse (pExpr <* space <* eof)


pExpr :: Parser Stack
pExpr = --dbg "expr" $
  Stack <$> (space *> (wrap pTerm `sepEndBy` space))

pLambda :: Parser (Node Stack)
pLambda = --dbg "lambda" $
  angles $ do
  argTys <- lexeme pType
  _      <- lexeme "|"
  body   <- pExpr
  pure $ DefSuperComb { superCombType = argTys
                      , body = body }
    where
      pType = do
        args   <- lexeme $ brackets $ lexeme $ pTypeTerm `sepBy1` lexeme ","
        result <- lexeme "->" *> pTypeTerm
        pure Func { args = args, result = result }

      pTypeTerm = I <$ "I" <|> pType

pTerm :: Parser (Node Stack)
pTerm = --dbg "term" $
  lexeme $ choice
  [ pLambda
  , StackedVal <$> pVal
  -- , RecStack <$> parens pExpr
  ]
