module Typing where

import           Shared                     (Node (..), Stack (..), Type (..),
                                             TypedStack (..), Val (..))

import           Control.Monad.Reader       (ReaderT, ask, local, runReader)
import           Control.Monad.State.Strict (StateT, get, modify, put)
import           Control.Monad.Trans        (lift)
import           Data.Bool                  (bool)
import           Data.IntMap                (IntMap)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Safe                       (atMay, headMay)
import           Text.Megaparsec            (ParseError, SourcePos)


stackNormalizeTo :: (SourcePos, Stack) -> ReaderT [Type] (Either SourcePos) (Type, TypedStack)
stackNormalizeTo (pos, stack) = do
  TypedStack tyStack <- stackType stack
  maybe (lift $ Left pos) (\ty -> pure (ty, TypedStack tyStack))
    $ tyListNormalizeTo $ deepReverse $ (\(_,t,_) -> t) <$> tyStack
    where
      deepReverse :: [Type] -> [Type]
      deepReverse (fTy@Func { args = args }:rest) =
        deepReverse rest <> [fTy { args = deepReverse args }]
      deepReverse (ty:rest) =
        deepReverse rest <> [ty]
      deepReverse [] =
        []

      tyListTruncTill :: [Type] -> Type -> Maybe [Type]
      tyListTruncTill (fTy@Func { args = arg0:args }:tys) target = do
        applyedOneArg <- tyListTruncTill tys arg0
        tyListTruncTill (fTy { args = args } : applyedOneArg) target
      tyListTruncTill (Func { args = [], result = ty }:tys) target =
        tyListTruncTill (ty:tys) target
      tyListTruncTill (ty:tys) target
        | ty == target = pure tys
        | otherwise    = Nothing
      tyListTruncTill [] target =
        Nothing

      tyListNormalizeTo :: [Type] -> Maybe Type
      tyListNormalizeTo (Func { result = result }:tys) = do
        shouldBeEmpty <- tyListTruncTill tys result
        case shouldBeEmpty of
          [] -> pure result
          _  -> Nothing
      tyListNormalizeTo [ty] = pure ty
      tyListNormalizeTo _    = Nothing

stackType :: Stack -> ReaderT [Type] (Either SourcePos) TypedStack
stackType (Stack stack) =
  TypedStack <$> nodeType `mapM` stack

nodeType :: (SourcePos, Node Stack) -> ReaderT [Type] (Either SourcePos) (SourcePos, Type, Node TypedStack)
nodeType (pos, DefSuperComb { superCombType = funcT@Func { args = env, result = expected }
                            , body = stack }) = do
   (resultT, tyStack) <- local (const env) $ stackNormalizeTo (pos, stack)
   if resultT == expected
     then pure (pos,funcT, DefSuperComb { superCombType = Func { args = env, result = resultT }
                                        , body = tyStack })
     else lift $ Left pos

nodeType (pos, DefSuperComb { superCombType = _ }) =
  lift $ Left pos
nodeType (pos, StackedVal Zero) =
  pure (pos, I, StackedVal Zero)
nodeType (pos, StackedVal (Bounded index)) = do
  tyList <- ask
  maybe (lift $ Left pos) (\t -> pure (pos, t, StackedVal (Bounded index)))
    $ atMay tyList (fromIntegral index)

nodeType (pos, StackedVal (SuccNotPred isSucc)) =
  pure (pos, Func { args = [I], result = I }, StackedVal (SuccNotPred isSucc))

