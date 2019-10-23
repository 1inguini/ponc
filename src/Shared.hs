module Shared where

import           Text.Megaparsec (SourcePos)

newtype Stack = Stack { unStack ::  [(SourcePos, Stackable)] }
              deriving (Eq)

instance Show Stack where
  show (Stack stack) = show $ snd <$> stack

data Type = I
          | Func { args :: [Type], result :: Type }
          deriving (Show, Eq)

data Stackable = DefSuperComb { superCombType :: Type, body :: Stack }
               | StackedVal Val
               | RecStack Stack
               deriving (Show, Eq)

data Val = -- IntVal Integer
         -- |
           Zero
         | Bounded Integer
         | SuccNotPred Bool
         deriving (Show, Ord, Eq)

type TypedStackable = (SourcePos, Type, Stackable)

newtype TypedStack = TypedStack { unTypeStack :: [TypedStackable] } deriving (Show, Eq)
