{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Syntax
  ( Expr(..)
  , Type(..)
  , Kind(..)
  )
where

import Bound.Scope (Scope)
import Bound.TH (makeBound)
import Control.Monad (ap)
import Data.Text (Text)
import Data.Vector (Vector)

data Expr a
  = Var a
  | Lam Text (Scope () Expr a)
  | App (Expr a) (Expr a)
  | Name Text

  | Quote (Expr a)
  | Antiquote (Expr a)
  | Splice (Expr a)

  | Int Int
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Functor, Foldable, Traversable)
makeBound ''Expr

data Type a
  = TVar a
  | TApp (Type a) (Type a)

  | TCode (Vector (Text, Type a))

  | TArr
  | TInt
  deriving (Functor, Foldable, Traversable)
instance Applicative Type where; pure = TVar; (<*>) = ap
instance Monad Type where
  t >>= f =
    case t of
      TVar a -> f a
      TApp a b -> TApp (a >>= f) (b >>= f)
      TCode a -> TCode $ (fmap.fmap) (>>= f) a

data Kind a
  = KVar a
  | KArr (Kind a) (Kind a)
  | KType
  deriving (Functor, Foldable, Traversable)
makeBound ''Kind
