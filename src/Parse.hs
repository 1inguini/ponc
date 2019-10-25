{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( filePathSrc2Stack ) where

import           Shared                     (ErrorBundle, FuncType (..),
                                             Node (..), NormType (..), Parser,
                                             Position, Stack (..), Type (..),
                                             Val (..))

import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseErrorBundle, between, choice,
                                             eof, getParserState, parse, sepBy1,
                                             sepEndBy, takeWhile1P, (<|>))
import           Text.Megaparsec            as P (State (..))
import qualified Text.Megaparsec.Char       as C (space1)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme,
                                                  skipBlockCommentNested,
                                                  skipLineComment, space)
import           Text.Megaparsec.Debug      (dbg)


filePathSrc2Stack :: FilePath -> Text -> Either ErrorBundle Stack
filePathSrc2Stack = parse (pExpr <* space <* eof)


wrap :: Parser a -> Parser (Position, a)
wrap p = (,) <$> getPosition <*> p
  where
    getPosition :: Parser Position
    getPosition = statePosState <$> getParserState

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


pExpr :: Parser Stack
pExpr = --dbg "expr" $
  Stack <$> (space *> (wrap pTerm `sepEndBy` space))

pLambda :: Parser (Node Stack)
pLambda = --dbg "lambda" $
  angles $ do
  argTys <- lexeme pFuncType
  _      <- lexeme "|"
  body   <- pExpr
  pure $ DefSuperComb { superCombType = argTys
                      , body = body }
    where
      pFuncType = do
        args   <- lexeme $ brackets $ lexeme $ pTypeTerm `sepBy1` lexeme ","
        result <- lexeme "->" *> pTypeTerm
        pure $ FuncType { args = args, result = result }

      pTypeTerm = Norm I <$ "I" <|> Func <$> pFuncType

pTerm :: Parser (Node Stack)
pTerm = --dbg "term" $
  lexeme $ choice
  [ pLambda
  , StackedVal <$> pVal
  -- , RecStack <$> parens pExpr
  ]
