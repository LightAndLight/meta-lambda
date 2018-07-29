{-# language TypeFamilies, OverloadedStrings, FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
module Syntax where

import Control.Applicative ((<|>), liftA2, many, some)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Setter (over)
import Data.Functor ((<$), ($>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Megaparsec
  (MonadParsec, Token, Tokens, ParseError, Parsec, SourcePos(..), unPos,
   between, parse, sepBy, getPosition)
import Text.Megaparsec.Char
  (char, notChar, upperChar, lowerChar, letterChar, spaceChar, newline,
   digitChar, string)
import Text.Megaparsec.Expr (makeExprParser, Operator(InfixL))

data SrcInfo
  = SrcInfo
  { _srcName :: String
  , _srcLine :: Int
  , _srcColumn :: Int
  } deriving (Eq, Show)

data Expr a
  = Var String (Maybe a)
  | Bound Int (Maybe a)
  | Lam (Expr a) (Maybe a)
  | Ctor String [Expr a] (Maybe a)
  | Unquote (Expr a) (Maybe a)
  | Quote (Expr a) (Maybe a)
  | App (Expr a) (Expr a) (Maybe a)
  | String String (Maybe a)
  | Int Int (Maybe a)
  deriving (Eq, Show, Generic)

instance Plated (Expr a) where; plate = gplate

abstract :: String -> Expr a -> Maybe a -> Expr a
abstract v e = Lam (fun 0 e)
  where
    fun !n t =
      case t of
        Var v' ann
          | v == v' -> Bound n ann
          | otherwise -> t
        Lam{} -> over plate (fun $ n+1) t
        _ -> over plate (fun n) t

exprAnn :: Lens' (Expr a) (Maybe a)
exprAnn =
  lens
    (\case
        Var _ a -> a
        Bound _ a -> a
        Lam _ a -> a
        Ctor _ _ a -> a
        Unquote _ a -> a
        Quote _ a -> a
        App _ _ a -> a
        String _ a -> a
        Int _ a -> a)
    (\e ann ->
       case e of
         Var a _ -> Var a ann
         Bound a _ -> Bound a ann
         Lam c _ -> Lam c ann
         Ctor a b _ -> Ctor a b ann
         Unquote a _ -> Unquote a ann
         Quote a _ -> Quote a ann
         App a b _ -> App a b ann
         String a _ -> String a ann
         Int a _ -> Int a ann)

data Decl a
  = Binding String (Expr a) (Maybe a)
  deriving (Eq, Show)

newtype Module a = Module [Decl a]
  deriving (Eq, Show)


expr :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (Expr SrcInfo)
expr = lam <|> app
  where
    lam = do
      SourcePos f line col <- getPosition
      (\name e ->
         abstract
           name
           e
           (Just $ SrcInfo f (unPos line) (unPos col))) <$
        char '\\' <*>
        liftA2 (:) lowerChar (many letterChar) <*
        string "->" <*>
        expr

    app =
      makeExprParser
        atom
        [[InfixL (some spaceChar $> (\a b -> App a b (a ^. exprAnn)))]]

    atom = do
      SourcePos name line col <- getPosition
      (Var <$> some letterChar <|>
       Quote <$ char '\'' <*> atom <|>
       Unquote <$ char '$' <*> atom <|>
       String <$> between (char '"') (char '"') (many (notChar '"')) <|>
       Int . read <$> some digitChar) <*>
        pure (Just $ SrcInfo name (unPos line) (unPos col)) <|>
        between (char '(') (char ')') expr

decl :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (Decl SrcInfo)
decl = do
  SourcePos name line col <- getPosition
  Binding <$>
    some letterChar <* char '=' <*>
    expr <*>
    pure (Just $ SrcInfo name (unPos line) (unPos col))

module_ :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (Module SrcInfo)
module_ = Module <$> sepBy decl newline

parseFile :: Parsec e String a -> FilePath -> IO (Either (ParseError Char e) a)
parseFile p fp = parse p fp <$> readFile fp

parseInteractive :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
parseInteractive p str = parse p "interactive" str

unquoteList :: (Expr SrcInfo -> a) -> Expr SrcInfo -> [a]
unquoteList f (Ctor "Nil" [] _) = []
unquoteList f (Ctor "Cons" [x, xs] _) = f x : unquoteList f xs
unquoteList _ a = error $ "can't unquote " ++ show a

unquote :: Expr SrcInfo -> Expr SrcInfo
unquote = go Nothing
  where
    go (Just n) (Bound n' a) | n' <= n = Bound n' a
    go inLambda (Ctor "Var" [String s _, _] ann) = Var s ann
    go inLambda (Ctor "Lam" [Lam e _, _] ann) =
      Lam (go ((+1) <$> inLambda <|> Just 0) e) ann
    go inLambda (Ctor "App" [f, x, _] ann) = App (go inLambda f) (go inLambda x) ann
    go inLambda (Ctor "String" [String s _, _] ann) = String s ann
    go inLambda (Ctor "Int" [Int i _, _] ann) = Int i ann
    go inLambda (Ctor "Ctor" [String s _, vs, _] ann) =
      Ctor s (unquoteList (go inLambda) vs) ann
    go inLambda e = error $ "can't unquote: " <> show e

quoteMaybe :: (a -> Expr b) -> Maybe a -> Expr b
quoteMaybe f Nothing = Ctor "Nothing" [] Nothing
quoteMaybe f (Just a) = Ctor "Just" [f a] Nothing

quoteList :: (a -> Expr b) -> [a] -> Expr b
quoteList f [] = Ctor "Nil" [] Nothing
quoteList f (a : as) = Ctor "Cons" [f a, quoteList f as] Nothing

quoteSrcInfo :: SrcInfo -> Expr b
quoteSrcInfo (SrcInfo name line col) =
  Ctor "SrcInfo"
    [ String name Nothing
    , Int line Nothing
    , Int col Nothing
    ]
    Nothing

quote :: Expr SrcInfo -> Expr SrcInfo
quote = go Nothing
  where
    go (Just n) (Bound n' a) | n' <= n = Bound n' a
    go inLambda (Var s ann) =
      Ctor "Var" [String s Nothing, quoteMaybe quoteSrcInfo ann] ann
    go inLambda (Lam e ann) =
      Ctor "Lam" [Lam (go ((+1) <$> inLambda <|> Just 0) e) Nothing, quoteMaybe quoteSrcInfo ann] ann
    go inLambda (App f x ann) =
      Ctor "App" [go inLambda f, go inLambda x, quoteMaybe quoteSrcInfo ann] ann
    go inLambda (String s ann) =
      Ctor "String" [String s Nothing, quoteMaybe quoteSrcInfo ann] ann
    go inLambda (Int i ann) =
      Ctor "Int" [Int i Nothing, quoteMaybe quoteSrcInfo ann] ann
    go inLambda (Ctor n vs ann) =
      Ctor "Ctor" [String n Nothing, quoteList (go inLambda) vs, quoteMaybe quoteSrcInfo ann] ann
    go inLambda e = error $ "can't quote: " <> show e

initialCtxt :: [(String, Expr a)]
initialCtxt =
  [ ( "Var"
    , Lam
        (Lam
           (Ctor "Var" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Lam"
    , Lam
        (Lam
           (Lam
              (Ctor "Lam"
                 [Bound 2 Nothing, Bound 1 Nothing, Bound 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "App"
    , Lam
        (Lam
           (Lam
              (Ctor "App"
                 [Bound 2 Nothing, Bound 1 Nothing, Bound 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "String"
    , Lam
        (Lam
           (Ctor "String" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Int"
    , Lam
        (Lam
           (Ctor "Int" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Nil"
    , Ctor "Nil" [] Nothing
    )
  , ( "Cons"
    , Lam
        (Lam
           (Ctor "Cons" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  ]

data Value a
  = VVar String (Maybe a)
  | VBound Int (Maybe a)
  | VLam [(String, Value a)] (Expr a) (Maybe a)
  | VCtor String [Value a] (Maybe a)
  | VString String (Maybe a)
  | VInt Int (Maybe a)
  deriving (Eq, Show)

eval :: [(String, Value SrcInfo)] -> Expr SrcInfo -> Value SrcInfo
eval ctxt = go ctxt [] . splices ctxt
  where
    go :: [(String, Value SrcInfo)] -> [Value SrcInfo] -> Expr SrcInfo -> Value SrcInfo
    go ctxt bound (Bound name _) = bound !! name
    go ctxt bound (Var name _) =
      case lookup name ctxt of
        Nothing -> error $ "stuck: name not in context"
        Just e -> e
    go ctxt bound (Lam expr a) =
      VLam ctxt expr a
    go ctxt bound (Ctor n es a) = VCtor n (go ctxt bound <$> es) a
    go ctxt bound (Unquote e _) = error "stuck: unquote not expanded"
    go ctxt bound (Quote e _) = go ctxt bound $ quote e
    go ctxt bound (App f x _) =
      let
        x' = go ctxt bound x
      in
        case go ctxt bound f of
          VLam ctxt' e _ -> go ctxt' (x' : bound) e
          _ -> error $ "stuck: application to non-function"
    go _ _ (String s a) = VString s a
    go _ _ (Int i a) = VInt i a

fromValue :: Value a -> Expr a
fromValue e =
  case e of
    VVar a b -> Var a b
    VBound a b -> Bound a b
    VLam _ b c -> Lam b c
    VCtor a b c -> Ctor a (fromValue <$> b) c
    VString a b -> String a b
    VInt a b -> Int a b

unquoteValue :: Value SrcInfo -> Expr SrcInfo
unquoteValue = unquote . fromValue

splices :: [(String, Value SrcInfo)] -> Expr SrcInfo -> Expr SrcInfo
splices ctxt (Lam expr a) = Lam (splices ctxt expr) a
splices ctxt (Ctor n es a) = Ctor n (splices ctxt <$> es) a
splices ctxt (Unquote e a) = unquoteValue $ eval ctxt e
splices ctxt (App f x a) = App (splices ctxt f) (splices ctxt x) a
splices ctxt e = e
