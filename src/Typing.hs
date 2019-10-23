module Typing where

import           Shared                     (Stack (..), Stackable (..),
                                             Type (..), TypedStack (..),
                                             TypedStackable, Val (..))

import           Control.Monad.Reader       (ReaderT, local, runReader)
import           Control.Monad.State.Strict (StateT, get, modify, put)
import           Control.Monad.Trans        (lift)
import           Data.IntMap                (IntMap)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseError, SourcePos)

stackNormalizeTo :: Stack -> ReaderT [Type] (Either SourcePos) TypedStackable
stackNormalizeTo (Stack (top:stacks)) = undefined
  where
    typeStack :: Stack -> ReaderT [Type] (Either SourcePos) TypedStack
    typeStack = undefined

    typeStackable :: (SourcePos, Stackable) -> ReaderT [Type] (Either SourcePos) TypedStackable
    -- typeStackable (_, DefSuperComb { superCombType = Func { args = env }, body = stack }) =
    --   local (const env) $ typeStack' stack
    -- typeStackable (pos, DefSuperComb { superCombType = _ , body = stack }) =
    --   lift $ Left pos
    typeStackable (pos, z@(StackedVal Zero)) =
      pure (pos, I, z)
    typeStackable (pos, s_p@(StackedVal (SuccNotPred _))) =
      pure (pos, Func { args = [I], result = I }, s_p)
    typeStackable (_, RecStack stack) =
      stackNormalizeTo stack

