{-# language TypeFamilies, OverloadedStrings, FlexibleContexts #-}
{-# language LambdaCase #-}
module Syntax where

import Control.Applicative ((<|>), liftA2, many, some)
import Control.Lens ((^.), Lens', lens)
import Data.Functor ((<$), ($>))
import Data.Semigroup ((<>))
import Data.String (IsString)
import Text.Megaparsec
  (MonadParsec, Token, Tokens, ParseError, Parsec, SourcePos(..), unPos,
   between, parse, sepBy, getPosition)
import Text.Megaparsec.Char
  (char, notChar, upperChar, lowerChar, letterChar, spaceChar, newline,
   digitChar, string)
import Text.Megaparsec.Expr (makeExprParser, Operator(InfixL))

import Debug.Trace

data SrcInfo
  = SrcInfo
  { _srcName :: String
  , _srcLine :: Int
  , _srcColumn :: Int
  } deriving (Eq, Show)

data Expr a
  = Var String (Maybe a)
  | Bound Int (Maybe a)
  | Lam [(String, Expr a)] (Maybe String) (Expr a) (Maybe a)
  | Ctor String [Expr a] (Maybe a)
  | Unquote (Expr a) (Maybe a)
  | Quote (Expr a) (Maybe a)
  | App (Expr a) (Expr a) (Maybe a)
  | String String (Maybe a)
  | Int Int (Maybe a)
  deriving (Eq, Show)

exprAnn :: Lens' (Expr a) (Maybe a)
exprAnn =
  lens
    (\case
        Var _ a -> a
        Bound _ a -> a
        Lam _ _ _ a -> a
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
         Lam a b c _ -> Lam a b c ann
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
      SourcePos name line col <- getPosition
      Lam [] . Just <$
        char '\\' <*>
        liftA2 (:) lowerChar (many letterChar) <*
        string "->" <*>
        expr <*>
        pure (Just $ SrcInfo name (unPos line) (unPos col))

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


unquote :: Expr SrcInfo -> Expr SrcInfo
unquote (Ctor "Var" [String s _, _] ann) = Var s ann
unquote (Ctor "Lam" [String s _, e, _] ann) = Lam [] (Just s) (unquote e) ann
unquote (Ctor "App" [f, x, _] ann) = App (unquote f) (unquote x) ann
unquote (Ctor "String" [String s _, _] ann) = String s ann
unquote (Ctor "Int" [Int i _, _] ann) = Int i ann
unquote e = error $ "can't unquote: " <> show e

quoteMaybe :: (a -> Expr b) -> Maybe a -> Expr b
quoteMaybe f Nothing = Ctor "Nothing" [] Nothing
quoteMaybe f (Just a) = Ctor "Just" [f a] Nothing

quoteSrcInfo :: SrcInfo -> Expr b
quoteSrcInfo (SrcInfo name line col) =
  Ctor "SrcInfo"
    [ String name Nothing
    , Int line Nothing
    , Int col Nothing
    ]
    Nothing

quote :: Expr SrcInfo -> Expr SrcInfo
quote (Var s ann) =
  Ctor "Var" [String s Nothing, quoteMaybe quoteSrcInfo ann] ann
quote (Lam _ (Just s) e ann) =
  Ctor "Lam" [String s Nothing, quote e, quoteMaybe quoteSrcInfo ann] ann
quote (App f x ann) =
  Ctor "App" [quote f, quote x, quoteMaybe quoteSrcInfo ann] ann
quote (String s ann) =
  Ctor "String" [String s Nothing, quoteMaybe quoteSrcInfo ann] ann
quote (Int i ann) =
  Ctor "Int" [Int i Nothing, quoteMaybe quoteSrcInfo ann] ann
quote e = error $ "can't quote: " <> show e

initialCtxt :: [(String, Expr a)]
initialCtxt =
  [ ( "Var"
    , Lam [] Nothing
        (Lam [] Nothing
           (Ctor "Var" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Lam"
    , Lam [] Nothing
        (Lam [] Nothing
           (Lam [] Nothing
              (Ctor "Lam"
                 [Bound 2 Nothing, Bound 1 Nothing, Bound 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "App"
    , Lam [] Nothing
        (Lam [] Nothing
           (Lam [] Nothing
              (Ctor "App"
                 [Bound 2 Nothing, Bound 1 Nothing, Bound 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "String"
    , Lam [] Nothing
        (Lam [] Nothing
           (Ctor "String" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Int"
    , Lam [] Nothing
        (Lam [] Nothing
           (Ctor "Int" [Bound 1 Nothing, Bound 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  ]

eval :: [(String, Expr SrcInfo)] -> Expr SrcInfo -> Expr SrcInfo
eval ctxt = go ctxt [] . splices ctxt
  where
    go ctxt bound (Bound name _) = bound !! traceShow bound name
    go ctxt bound (Var name _) =
      case lookup name ctxt of
        Nothing -> error $ "stuck: name not in context"
        Just e -> e
    go ctxt bound (Lam _ n expr a) =
      Lam ctxt n (_ expr) a
    go ctxt bound (Ctor n es a) = Ctor n (go ctxt bound <$> es) a
    go ctxt bound (Unquote e _) = error "stuck: unquote not expanded"
    go ctxt bound (Quote e _) = go ctxt bound $ quote e
    go ctxt bound (App f x _) =
      let
        x' = go ctxt bound x
      in
        case go ctxt bound f of
          Lam ctxt' (Just n) e _ -> go ((n, x') : ctxt') (x' : bound) e
          Lam ctxt' Nothing e _ -> go ctxt' (x' : bound) e
          _ -> error $ "stuck: application to non-function"
    go _ bound (String s a) = String s a
    go _ bound (Int i a) = Int i a

splices :: [(String, Expr SrcInfo)] -> Expr SrcInfo -> Expr SrcInfo
splices ctxt (Lam ctxt' n expr a) = Lam ctxt' n (splices ctxt expr) a
splices ctxt (Ctor n es a) = Ctor n (splices ctxt <$> es) a
splices ctxt (Unquote e a) = unquote $ eval ctxt e
splices ctxt (App f x a) = App (splices ctxt f) (splices ctxt x) a
splices ctxt e = e
