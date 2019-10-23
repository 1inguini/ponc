module Shared where

import           Text.Megaparsec (SourcePos)

newtype Stack = Stack { unStack :: [(SourcePos, Node Stack)] }
              deriving (Eq)

instance Show Stack where
  show (Stack stack) = show $ snd <$> stack

data Type = I
          | Func { args :: [Type], result :: Type }
          deriving (Show, Eq)

data Node s = DefSuperComb { superCombType :: Type, body :: s }
            | StackedVal Val
            deriving (Show, Eq)

data Val = -- IntVal Integer
         -- |
           Zero
         | Bounded Integer
         | SuccNotPred Bool
         -- | FixPoint
         deriving (Show, Ord, Eq)


newtype TypedStack = TypedStack { unTypeStack :: [(SourcePos, Type, Node TypedStack)] } deriving (Show, Eq)
