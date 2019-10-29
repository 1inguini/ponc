module Typing
  ( stack2typedStack
  ) where

import qualified Data.List.NonEmpty as NE
import           Data.Set           (singleton)
import           Shared             (ErrorBundle, FuncType (..), Node (..),
                                     NonParserError (..), NormType (..),
                                     Position, ReversedNE (..), Stack (..),
                                     Type (..), TypedStack (..), Val (..),
                                     atMay, toList)
import           Text.Megaparsec    (ErrorFancy (..), ParseError (..),
                                     ParseErrorBundle (..), PosState (..))

type Failable = Either ErrorBundle

stack2typedStack :: Stack -> Failable TypedStack
stack2typedStack (Stack stack) =
  TypedStack <$> typeNode Nothing `mapM` stack


throwError :: Position -> NonParserError -> Failable a
throwError pos@PosState { pstateOffset = offset } e = Left $ ParseErrorBundle {
  bundleErrors = FancyError offset (singleton (ErrorCustom e)) NE.:| []
  , bundlePosState = pos
  }


typeNode :: Maybe FuncType
         -> (Position, Node Stack)
         -> Failable (Position, Type, Node TypedStack)
typeNode _ (pos, DefSuperComb { superCombType = ty, body = stack }) = do
  typedStack <- TypedStack <$> typeNode (Just ty) `mapM` unStack stack
  pure (pos, Func ty, DefSuperComb { superCombType = ty, body = typedStack })
typeNode _ (pos, StackedVal Zero) =
  pure (pos, Norm I, StackedVal Zero)
typeNode _ (pos, StackedVal (SuccNotPred isSucc)) =
  pure ( pos
       , Func FuncType { args = Head (Norm I), result = I }
       , StackedVal (SuccNotPred isSucc) )
typeNode (Just FuncType { args = env }) (pos, StackedVal (Bounded index)) =
  maybe
  (throwError pos
    UnboundedArg { expectedArgs = toList env, actualArg = index })
  (\ty ->
      pure (pos, ty, StackedVal (Bounded index)))
  $ atMay env index
typeNode Nothing (pos, StackedVal (Bounded index)) =
  throwError pos
  UnboundedArg { expectedArgs = [], actualArg = index }
