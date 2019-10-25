module Typing
  ( stack2typedStack
  ) where

import           Shared               (ErrorBundle, FuncType (..), Node (..),
                                       NonParserError (..), NormType (..),
                                       Position, Stack (..), Type (..),
                                       TypedStack (..), Val (..))

import           Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans  (lift)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Set             (singleton)
import           Safe                 (atMay)
import           Text.Megaparsec      (ErrorFancy (..), ParseError (..),
                                       ParseErrorBundle (..), PosState (..))

stack2typedStack :: Stack -> Either ErrorBundle TypedStack
stack2typedStack stack = runReaderT (stackType stack) ([], Norm I)

throwError :: Position -> NonParserError -> Either ErrorBundle a
throwError pos@PosState { pstateOffset = offset } e = Left $ ParseErrorBundle {
  bundleErrors = FancyError offset (singleton (ErrorCustom e)) :| []
  , bundlePosState = pos
  }

stackNormalizeTo :: (Position, Stack) -> ReaderT ([Type], Type) (Either ErrorBundle) (Type, TypedStack)
stackNormalizeTo (pos, stack) = do
  TypedStack tyStack  <- stackType stack
  (_, expectedResult) <- ask
  ty                  <- lift $ tyListNormalizeTo $ deepReverse $ (\(p,t,_) -> (p,t)) <$> tyStack
  pure (ty, TypedStack tyStack)
    where
      deepReverse :: [(Position, Type)] -> [(Position, Type)]
      deepReverse ((pos, Func fTy@FuncType { args = args }):rest) =
        deepReverse rest <> [(pos, Func fTy { args = reverse args })]
      deepReverse (ty:rest) =
        deepReverse rest <> [ty]
      deepReverse [] =
        []

      tyListTruncTill :: [(Position, Type)] -> Type ->  Either ErrorBundle [(Position, Type)]
      tyListTruncTill ((pos, Func fTy@FuncType { args = arg0:args }):tys) target = do
        applyedOneArg <- tyListTruncTill tys arg0
        tyListTruncTill ((pos, Func fTy { args = args }) : applyedOneArg) target
      tyListTruncTill ((pos, Func FuncType { args = [], result = ty }):tys) target =
        tyListTruncTill ((pos, ty):tys) target
      tyListTruncTill ((pos, ty):tys) target
        | ty == target = pure tys
        | otherwise    = throwError pos TypeMismatch { expectedType = target, actualType = ty }
      tyListTruncTill [] target =
        throwError pos LackArgs

      tyListNormalizeTo :: [(Position, Type)] -> Either ErrorBundle Type
      tyListNormalizeTo ((_, Func FuncType { result = result }):tys) = do
        shouldBeEmpty <- tyListTruncTill tys result
        case shouldBeEmpty of
          []         -> pure result
          (pos, _):_ -> throwError pos ExcessArgs
      tyListNormalizeTo [(_, ty)] = pure ty
      -- tyListNormalizeTo _    = Nothing

stackType :: Stack -> ReaderT ([Type], Type) (Either ErrorBundle) TypedStack
stackType (Stack stack) =
  TypedStack <$> nodeType `mapM` stack

nodeType :: (Position, Node Stack) -> ReaderT ([Type], Type) (Either ErrorBundle) (Position, Type, Node TypedStack)
nodeType (pos, DefSuperComb { superCombType = funcT@FuncType { args = env, result = expected }
                            , body = stack }) = do
   (resultT, tyStack) <- local (const (env, expected)) $ stackNormalizeTo (pos, stack)
   if resultT == expected
     then pure (pos, Func funcT
               , DefSuperComb { superCombType = FuncType { args = env, result = resultT }
                              , body = tyStack })
     else lift $ throwError pos TypeMismatch { expectedType = expected, actualType = resultT }

nodeType (pos, StackedVal Zero) =
  pure (pos, Norm I, StackedVal Zero)
nodeType (pos, StackedVal (Bounded index)) = do
  (tyList, _) <- ask
  maybe (lift $ throwError pos UnboundedArg { expectedArgs = tyList
                                            , actualArg = index })
    (\t -> pure (pos, t, StackedVal (Bounded index)))
    $ atMay tyList (fromIntegral index)

nodeType (pos, StackedVal (SuccNotPred isSucc)) =
  pure (pos, Func FuncType { args = [Norm I], result = Norm I }, StackedVal (SuccNotPred isSucc))

