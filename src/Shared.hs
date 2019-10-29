module Shared where

import           Data.List        (intercalate)
import           Data.Text        (Text)
-- import qualified Data.Text          as Text
import           Data.Traversable (foldMapDefault)
import           Text.Megaparsec  (ParseErrorBundle (..), Parsec, PosState (..),
                                   ShowErrorComponent (..))
data ReversedNE a = Head a
                | ReversedNE a :| a
                deriving (Eq, Ord)

instance (Show a) => Show (ReversedNE a) where
  show = show . toList

instance Functor ReversedNE where
  fmap f (xs :| x) = fmap f xs :| f x
  fmap f (Head x)  = Head $ f x

instance Foldable ReversedNE where
  foldMap = foldMapDefault

instance Traversable ReversedNE where
  traverse f (xs :| x) = (:|) <$> traverse f xs <*> f x
  traverse f (Head x)  = Head <$> f x


toList :: ReversedNE a -> [a]
toList (xs :| x) = toList xs <> [x]
toList (Head x)  = [x]

fromList :: a -> [a] -> ReversedNE a
fromList last (x:xs) = fromList x xs :| last
fromList last []     = Head last

atMay :: Integral i => ReversedNE a -> i -> Maybe a
atMay xs o | o < 0 = Nothing
           | otherwise = f o xs
  where f 0 (_:|x)   = Just x
        f 0 (Head x) = Just x
        f i (xs:|_)  = f (pred i) xs
        f _ (Head _) = Nothing


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
          $ (\(i,ty) -> "$" <> show i <> " : " <> show ty) <$> zip [0..] expected
    in "expected one of " <> showExpected <> " but got $" <> show actual
  showErrorComponent ExcessArgs =
    "excess args"
  showErrorComponent LackArgs =
    "lacks args"

type Position = PosState Text

newtype Stack = Stack { unStack :: ReversedNE (Position, Node Stack) }
              deriving (Eq)

instance Show Stack where
  show (Stack stack) = show $ snd <$> stack

data Type = Norm NormType
          | Func FuncType
          deriving (Eq, Ord)

data FuncType = FuncType { args   :: ReversedNE Type
                         , result :: NormType
                         } deriving (Show, Eq, Ord)

data NormType = I deriving (Eq, Ord)

instance  Show NormType where
  show I = "I"

instance Show Type where
  show (Norm a)                                        = show a
  show (Func FuncType { args = tys, result = result }) =
    show (toList tys) <> " -> " <> show result


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


newtype TypedStack = TypedStack { unTypeStack :: ReversedNE (Position, Type, Node TypedStack) } deriving (Eq)

instance Show TypedStack where
  show (TypedStack tyStack) = show $ (\(_,ty,node) -> (ty,node)) <$> tyStack
