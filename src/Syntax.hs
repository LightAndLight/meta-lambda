{-# language TypeFamilies, OverloadedStrings, FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
module Syntax where

import Control.Applicative ((<|>), liftA2, many, some, empty)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Setter (over)
import Data.Functor ((<$), ($>))
import Data.List (foldl')
import Data.Semigroup ((<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Megaparsec
  (MonadParsec, Token, Tokens, ParseError, Parsec, SourcePos(..), unPos,
   between, parse, sepBy, sepEndBy, getPosition, try)
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

data Lit a
  = StringL String (Maybe a)
  | IntL Int (Maybe a)
  deriving (Eq, Show)

data Pattern a
  = WildP (Maybe a)
  | LitP (Lit a)
  | CtorP String Int (Maybe a)
  deriving (Eq, Show)

data Branch a = Branch (Pattern a) (Expr a) (Maybe a)
  deriving (Eq, Show, Generic)

data Expr a
  = VarE String (Maybe a)
  | BoundE !Int (Maybe a)
  | LamE (Expr a) (Maybe a)
  | CtorE String [Expr a] (Maybe a)
  | UnquoteE (Expr a) (Maybe a)
  | QuoteE (Expr a) (Maybe a)
  | LitE (Lit a)
  | AppE (Expr a) (Expr a) (Maybe a)
  | CaseE (Expr a) [Branch a] (Maybe a)
  deriving (Eq, Show, Generic)

instance Plated (Expr a) where
  {-# inline plate #-}
  plate f (CaseE a b c) =
    CaseE <$>
    f a <*>
    traverse (\(Branch a b c) -> Branch a <$> f b <*> pure c) b <*>
    pure c
  plate f a = gplate f a

abstract :: String -> Expr a -> Maybe a -> Expr a
abstract v e = LamE (fun 0 e)
  where
    fun !n t =
      case t of
        VarE v' ann
          | v == v' -> BoundE n ann
          | otherwise -> t
        LamE{} -> over plate (fun $ n+1) t
        _ -> over plate (fun n) t

subst :: Expr a -> Expr a -> Expr a
subst new = fun 0
  where
    fun !n t =
      case t of
        BoundE n' ann
          | n == n' -> new
          | otherwise -> BoundE (n'-1) ann
        LamE{} -> over plate (fun $ n+1) t
        _ -> over plate (fun n) t

exprAnn :: Lens' (Expr a) (Maybe a)
exprAnn =
  lens
    (\case
        CaseE _ _ a -> a
        VarE _ a -> a
        BoundE _ a -> a
        LamE _ a -> a
        CtorE _ _ a -> a
        UnquoteE _ a -> a
        QuoteE _ a -> a
        AppE _ _ a -> a
        LitE (StringL _ a) -> a
        LitE (IntL _ a) -> a)
    (\e ann ->
       case e of
         CaseE a b _ -> CaseE a b ann
         VarE a _ -> VarE a ann
         BoundE a _ -> BoundE a ann
         LamE c _ -> LamE c ann
         CtorE a b _ -> CtorE a b ann
         UnquoteE a _ -> UnquoteE a ann
         QuoteE a _ -> QuoteE a ann
         AppE a b _ -> AppE a b ann
         LitE (StringL a _) -> LitE $ StringL a ann
         LitE (IntL a _) -> LitE $ IntL a ann)

data Decl a
  = Binding String (Expr a) (Maybe a)
  deriving (Eq, Show)

newtype Module a = Module [Decl a]
  deriving (Eq, Show)

token :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
token p = p <* many (char ' ' <|> char '\n')

expr :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (Expr SrcInfo)
expr = lam <|> case_ <|> token app
  where
    lam = do
      SourcePos f line col <- getPosition
      (\name e ->
         abstract
           name
           e
           (Just $ SrcInfo f (unPos line) (unPos col))) <$
        token (char '\\') <*>
        token (liftA2 (:) lowerChar (many letterChar)) <*
        token (string "->") <*>
        expr

    pattern_ = do
      SourcePos f line col <- getPosition
      let srcInfo = Just $ SrcInfo f (unPos line) (unPos col)
      (WildP srcInfo, id) <$ token (char '_') <|>
        flip (,) id . LitP <$> token lit <|>
        (\tag args -> (CtorP tag (length args) srcInfo, \x -> foldr (\a b -> abstract a b Nothing) x args)) <$>
        token (some letterChar) <*>
        many (token (some letterChar))

    branch = do
      SourcePos f line col <- getPosition
      (\(pat, doAbstract) rhs -> Branch pat (doAbstract rhs) (Just $ SrcInfo f (unPos line) (unPos col))) <$>
        pattern_ <* token (string "->") <*>
        token expr

    branches = sepEndBy branch (token $ char ';')

    case_ = do
      SourcePos f line col <- getPosition
      (\e bs -> CaseE e bs (Just $ SrcInfo f (unPos line) (unPos col))) <$
        token (string "case") <*>
        token atom <* token (string "of") <*>
        between (token $ char '{') (char '}') branches

    app =
      foldl' (\b a -> AppE b a (b ^. exprAnn)) <$>
      atom <*>
      many (try $ some spaceChar *> atom)

    lit = do
      SourcePos f line col <- getPosition
      let srcInfo = Just $ SrcInfo f (unPos line) (unPos col)
      (StringL <$> between (char '"') (char '"') (many (notChar '"')) <|>
        IntL . read <$> some digitChar) <*>
        pure srcInfo

    keywords = ["case", "of"]

    atom = do
      SourcePos name line col <- getPosition
      let srcPos = Just $ SrcInfo name (unPos line) (unPos col)
      (VarE <$> try (some letterChar >>= \x -> if x `elem` keywords then empty else pure x) <*> pure srcPos <|>
       QuoteE <$ token (char '\'') <*> atom <*> pure srcPos <|>
       UnquoteE <$ token (char '$') <*> atom <*> pure srcPos <|>
       LitE <$> lit) <|>
        between (token (char '(')) (char ')') (token expr)

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
unquoteList f (CtorE "Nil" [] _) = []
unquoteList f (CtorE "Cons" [x, xs] _) = f x : unquoteList f xs
unquoteList _ a = error $ "can't unquote " ++ show a

unquote :: Expr SrcInfo -> Expr SrcInfo
unquote = go Nothing
  where
    go (Just n) (BoundE n' a) | n' <= n = BoundE n' a
    go inLambda (CtorE "Var" [LitE (StringL s _), _] ann) = VarE s ann
    go inLambda (CtorE "Lam" [LamE e _, _] ann) =
      LamE (go ((+1) <$> inLambda <|> Just 0) e) ann
    go inLambda (CtorE "App" [f, x, _] ann) = AppE (go inLambda f) (go inLambda x) ann
    go inLambda (CtorE "String" [LitE (StringL s _), _] ann) = LitE $ StringL s ann
    go inLambda (CtorE "Int" [LitE (IntL i _), _] ann) = LitE $ IntL i ann
    go inLambda (CtorE "Ctor" [LitE (StringL s _), vs, _] ann) =
      CtorE s (unquoteList (go inLambda) vs) ann
    go inLambda e = error $ "can't unquote: " <> show e

quoteMaybe :: (a -> Expr b) -> Maybe a -> Expr b
quoteMaybe f Nothing = CtorE "Nothing" [] Nothing
quoteMaybe f (Just a) = CtorE "Just" [f a] Nothing

quoteList :: (a -> Expr b) -> [a] -> Expr b
quoteList f [] = CtorE "Nil" [] Nothing
quoteList f (a : as) = CtorE "Cons" [f a, quoteList f as] Nothing

quoteSrcInfo :: SrcInfo -> Expr b
quoteSrcInfo (SrcInfo name line col) =
  CtorE "SrcInfo"
    [ LitE $ StringL name Nothing
    , LitE $ IntL line Nothing
    , LitE $ IntL col Nothing
    ]
    Nothing

quoteLit :: Lit SrcInfo -> Expr SrcInfo
quoteLit (StringL s ann) =
  CtorE "StringL" [LitE (StringL s ann)] Nothing
quoteLit (IntL i ann) =
  CtorE "IntL" [LitE (IntL i ann)] Nothing

quote :: Expr SrcInfo -> Expr SrcInfo
quote = go Nothing
  where
    go (Just n) (BoundE n' a) | n' <= n = BoundE n' a
    go inLambda (VarE s ann) =
      CtorE "Var" [LitE (StringL s Nothing), quoteMaybe quoteSrcInfo ann] Nothing
    go inLambda (LamE e ann) =
      CtorE "Lam" [LamE (go ((+1) <$> inLambda <|> Just 0) e) Nothing, quoteMaybe quoteSrcInfo ann] Nothing
    go inLambda (AppE f x ann) =
      CtorE "App" [go inLambda f, go inLambda x, quoteMaybe quoteSrcInfo ann] Nothing
    go inLambda (LitE l) =
      CtorE "Lit" [quoteLit l] Nothing
    go inLambda (CtorE n vs ann) =
      CtorE "Ctor" [LitE (StringL n Nothing), quoteList (go inLambda) vs, quoteMaybe quoteSrcInfo ann] Nothing
    go inLambda (CaseE e bs ann) =
      CtorE "Case" [quote e, quoteList (quoteBranch inLambda) bs, quoteMaybe quoteSrcInfo ann] Nothing
      where
        quoteBranch il (Branch pat rhs ann) =
          CtorE "Branch" [quotePattern pat, go il rhs, quoteMaybe quoteSrcInfo ann] Nothing

        quotePattern (WildP ann) =
          CtorE "WildP" [quoteMaybe quoteSrcInfo ann] Nothing
        quotePattern (LitP l) =
          CtorE "LitP" [quoteLit l] Nothing
        quotePattern (CtorP tag nums ann) =
          CtorE "CtorP" [LitE (StringL tag Nothing), _ nums, quoteMaybe quoteSrcInfo ann] Nothing

    go inLambda e = error $ "can't quote: " <> show e

data DataInfo = DataInfo { _arity :: !Int }
  deriving (Eq, Show)

initialDataInfo :: [(String, DataInfo)]
initialDataInfo =
  [ ("Var", DataInfo 2)
  , ("Lam", DataInfo 3)
  , ("App", DataInfo 3)
  , ("String", DataInfo 2)
  , ("Int", DataInfo 2)
  , ("Nil", DataInfo 0)
  , ("Cons", DataInfo 2)
  ]

initialCtxt :: [(String, Value a)]
initialCtxt =
  [ ( "Var"
    , VLam []
        (LamE
           (CtorE "Var" [BoundE 1 Nothing, BoundE 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Lam"
    , VLam []
        (LamE
           (LamE
              (CtorE "Lam"
                 [BoundE 2 Nothing, BoundE 1 Nothing, BoundE 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "App"
    , VLam []
        (LamE
           (LamE
              (CtorE "App"
                 [BoundE 2 Nothing, BoundE 1 Nothing, BoundE 0 Nothing]
                 Nothing)
              Nothing)
           Nothing)
        Nothing
    )
  , ( "String"
    , VLam []
        (LamE
           (CtorE "String" [BoundE 1 Nothing, BoundE 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Int"
    , VLam []
        (LamE
           (CtorE "Int" [BoundE 1 Nothing, BoundE 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  , ( "Nil"
    , VCtor "Nil" [] Nothing
    )
  , ( "Cons"
    , VLam []
        (LamE
           (CtorE "Cons" [BoundE 1 Nothing, BoundE 0 Nothing] Nothing)
           Nothing)
        Nothing
    )
  ]

data Value a
  = VVar String (Maybe a)
  | VBound Int (Maybe a)
  | VLam [(String, Value a)] (Expr a) (Maybe a)
  | VCtor String [Value a] (Maybe a)
  | VLit (Lit a)
  deriving (Eq, Show)

eval :: [(String, DataInfo)] -> [(String, Value SrcInfo)] -> Expr SrcInfo -> Value SrcInfo
eval dInfo ctxt = go ctxt [] . splices dInfo ctxt
  where
    go
      :: [(String, Value SrcInfo)]
      -> [Value SrcInfo]
      -> Expr SrcInfo
      -> Value SrcInfo
    go ctxt bound (CaseE e bs _) =
      let
        e' = go ctxt bound e
      in
        foldr
          (\(Branch pat rhs _) b ->
             case pat of
               WildP _ -> go ctxt bound rhs
               LitP l | VLit l' <- e', l == l' -> go ctxt bound rhs
               CtorP tag binds _ | VCtor tag' vs _ <- e', tag == tag' ->
                 if binds == length vs
                 then go ctxt (reverse vs ++ bound) rhs
                 else error "stuck: arity mismatch in pattern"
               _ -> b)
          (error "stuck: pattern does not match")
          bs
    go ctxt bound (BoundE name _) = bound !! name
    go ctxt bound (VarE name _) =
      case lookup name ctxt of
        Nothing -> error $ "stuck: name not in context"
        Just e -> e
    go ctxt bound (LamE expr a) =
      VLam ctxt expr a
    go ctxt bound (CtorE n es a) = VCtor n (go ctxt bound <$> es) a
    go ctxt bound (UnquoteE e _) = error "stuck: unquote not expanded"
    go ctxt bound (QuoteE e _) = go ctxt bound $ quote e
    go ctxt bound (AppE f x _) =
      let
        x' = go ctxt bound x
      in
        case go ctxt bound f of
          VLam ctxt' e _ -> go ctxt' (x' : bound) e
          _ -> error $ "stuck: application to non-function"
    go _ _ (LitE l) = VLit l

fromValue :: Value a -> Expr a
fromValue e =
  case e of
    VVar a b -> VarE a b
    VBound a b -> BoundE a b
    VLam _ b c -> LamE b c
    VCtor a b c -> CtorE a (fromValue <$> b) c
    VLit l -> LitE l

unquoteValue :: Value SrcInfo -> Expr SrcInfo
unquoteValue = unquote . fromValue

splices :: [(String, DataInfo)] -> [(String, Value SrcInfo)] -> Expr SrcInfo -> Expr SrcInfo
splices dInfo ctxt (LamE expr a) = LamE (splices dInfo ctxt expr) a
splices dInfo ctxt (CtorE n es a) = CtorE n (splices dInfo ctxt <$> es) a
splices dInfo ctxt (UnquoteE e a) = unquoteValue $ eval dInfo ctxt e
splices dInfo ctxt (AppE f x a) = AppE (splices dInfo ctxt f) (splices dInfo ctxt x) a
splices dInfo ctxt e = e
