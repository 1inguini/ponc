module Shared where

import           Data.List       (intercalate)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Megaparsec (ParseErrorBundle (..), Parsec, PosState (..),
                                  ShowErrorComponent (..), SourcePos)

type Parser = Parsec NonParserError Text

type ErrorBundle = ParseErrorBundle Text NonParserError

data NonParserError = TypeMismatch { expectedType :: Type
                                   , actualType   :: Type }
                    | UnboundedArg { expectedArgs :: [Type]
                                   , actualArg    :: Integer }
                    | ExcessArgs
                    | LackArgs
                    deriving (Show, Eq, Ord)

instance ShowErrorComponent NonParserError where
  showErrorComponent TypeMismatch { expectedType = expected, actualType = actual } =
    "expected type " <> show expected <> " but got " <> show actual
  showErrorComponent UnboundedArg { expectedArgs = expected, actualArg = actual } =
    let showExpected = intercalate " or "
          $ (\(i,ty) -> "$" <> show i <> " : " <> unparseType ty) <$> zip [0..] expected
    in "expected one of " <> showExpected <> " but got $" <> show actual
  showErrorComponent ExcessArgs =
    "excess args"
  showErrorComponent LackArgs =
    "lacks args"

type Position = PosState Text

newtype Stack = Stack { unStack :: [(Position, Node Stack)] }
              deriving (Eq)

instance Show Stack where
  show (Stack stack) = show $ snd <$> stack

data Type = Norm NormType
          | Func FuncType
          deriving (Show, Eq, Ord)

data FuncType = FuncType { args :: [Type], result :: Type } deriving (Show, Eq, Ord)

data NormType = I deriving (Show, Eq, Ord)

unparseType :: Type -> String
unparseType (Norm I)                                        = "I"
unparseType (Func FuncType { args = tys, result = result }) =
  "[ " <> intercalate ", " (unparseType <$> tys) <> " ] -> " <> unparseType result


data Node s = DefSuperComb { superCombType :: FuncType, body :: s }
            | StackedVal Val
            deriving (Show, Eq)

data Val = -- IntVal Integer
         -- |
           Zero
         | Bounded Integer
         | SuccNotPred Bool
         -- | FixPoint
         deriving (Show, Ord, Eq)


newtype TypedStack = TypedStack { unTypeStack :: [(Position, Type, Node TypedStack)] } deriving (Eq)

instance Show TypedStack where
  show (TypedStack tyStack) = show $ (\(_,ty,node) -> (ty,node)) <$> tyStack
