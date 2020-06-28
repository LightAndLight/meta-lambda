{-# language OverloadedLists #-}
module Check.Type
  ( check
  , infer
  )
where

import Bound (fromScope)
import Bound.Var (unvar)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import Syntax (Expr(..), Type(..))

data TypeError
  = TypeMismatch (Type Text) (Type Text)

  | LamIsn't (Type Text)

  | ExpectedFunction (Type Text)
  | ExpectedCode (Maybe (Vector (Text, Type Text))) (Type Text)

  | NotInScope Text

typeMismatch :: (ty -> Text) -> Type ty -> Type ty -> Either TypeError a 
typeMismatch tyNames t t' = Left $ TypeMismatch (tyNames <$> t) (tyNames <$> t')

check ::
  Eq ty =>
  (ty -> Text) ->
  Map Text (Type ty) ->
  (tm -> Type ty) ->
  Expr tm ->
  Type ty ->
  Either TypeError ()
check tyNames globalTypes types e t =
  case e of
    Lam n body ->
      case t of
        TApp (TApp TArr inTy) outTy ->
          check tyNames globalTypes (unvar (\() -> inTy) types) (fromScope body) outTy
        _ -> Left $ LamIsn't (tyNames <$> t)

infer ::
  Eq ty =>
  (ty -> Text) ->
  Map Text (Type ty) ->
  (tm -> Type ty) ->
  Expr tm ->
  Either TypeError (Type ty)
infer tyNames globalTypes types e =
  case e of
    Var a -> pure $ types a
    App f x -> do
      fTy <- infer tyNames globalTypes types f
      case fTy of
        TApp (TApp TArr inTy) outTy ->
          outTy <$ check tyNames globalTypes types x inTy
        _ -> Left $ ExpectedFunction (tyNames <$> fTy)
    Name n ->
      maybe (Left $ NotInScope n) Right $
      Map.lookup n globalTypes

    Int{} -> pure TInt
    Add a b -> do
      check tyNames globalTypes types a TInt
      check tyNames globalTypes types b TInt
      pure TInt
    Mul a b -> do
      check tyNames globalTypes types a TInt
      check tyNames globalTypes types b TInt
      pure TInt

    Quote body -> do
      bodyTy <- _
      bodyCtx <- _
      pure $ TApp (TCode bodyCtx) bodyTy
    Antiquote body -> do
      bodyTy <- infer tyNames globalTypes types body
      case bodyTy of
        TApp (TCode ctx) ty -> _
        _ -> Left $ ExpectedCode Nothing (tyNames <$> bodyTy)
    Splice body -> do
      bodyTy <- infer tyNames globalTypes types body
      case bodyTy of
        TApp (TCode []) ty -> pure ty
        _ -> Left $ ExpectedCode (Just []) (tyNames <$> bodyTy)
