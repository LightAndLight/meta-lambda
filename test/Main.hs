{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language BangPatterns #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Traversable (for)
import Data.Void (Void)

import Hedgehog
  ((===), Property, MonadGen, Size(..), property, discover, forAll
  , checkSequential, annotateShow
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Syntax

genCtxt :: MonadGen m => m [(String, Value SrcInfo)]
genCtxt = do
  n <- Gen.integral (Range.constant 0 20)
  names <-
    nub <$>
    Gen.list
    (Range.singleton n)
    (Gen.string (Range.constant 1 10) Gen.ascii)
  go n [] names
  where
    go n seen [] = pure seen
    go n seen (x : xs) = do
      e <- Gen.scale (`div` Size n) (genExpr seen)
      go n ((x, eval seen e) : seen) xs

genExpr :: MonadGen m => [(String, Value SrcInfo)] -> m (Expr SrcInfo)
genExpr ctxt = go ctxt []
  where
    genLam ctxt bound = Lam <$> go ctxt (length bound : bound)
    go ctxt bound =
      Gen.recursive
        Gen.choice
        ([ Var <$> Gen.element (fst <$> ctxt) | not (null ctxt) ] ++
         [ Bound <$> Gen.element bound | not (null bound) ] ++
         [ String <$>
           Gen.string (Range.constant 0 10) Gen.ascii
         , Int <$>
           Gen.int (Range.constant (-100) 100)
         ])
         [ genLam ctxt bound
         , do
             n <- Gen.integral (Range.constant 0 5)
             Ctor <$>
               Gen.string (Range.constant 1 10) Gen.ascii <*>
               Gen.list
                 (Range.singleton n)
                 (Gen.scale (`div` Size n) $ go ctxt bound)
         , App <$>
           (Gen.scale (`div` 2) (genLam ctxt bound) <*> pure Nothing) <*>
           Gen.scale (`div` 2) (go ctxt bound)
         ] <*>
        pure Nothing

prop_quote_unquote :: Property
prop_quote_unquote =
  property $ do
    ctxt <- forAll genCtxt
    expr <- forAll $ genExpr ctxt
    fromValue (eval ctxt $ Unquote (Quote expr Nothing) Nothing) ===
      fromValue (eval ctxt expr)

main :: IO Bool
main = checkSequential $$(discover)
