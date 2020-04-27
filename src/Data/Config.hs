{-# LANGUAGE UndecidableInstances #-}

module Data.Config where

import GHC.TypeLits
import GHC.Generics
import GHC.Exts
import Data.Proxy
import System.Environment
import Control.Monad.IO.Class
import Data.Maybe
import Control.Exception

data FieldType = Required | Optional

data ConfigIdentity (t :: FieldType) (env :: Symbol) a

data Defaults (t :: FieldType) (env :: Symbol) a where
  NoDefault :: Defaults 'Required env a
  Default :: a -> Defaults 'Optional env a

data Parser m (t :: FieldType) (env :: Symbol) a where
  RequiredParser :: m a -> Parser m 'Required env a
  OptionalParser :: m (Maybe a) -> Parser m 'Optional env a

class DefaultParser m t env a where
  defaultFieldParser :: Parser m t env a

class ParseField a where
  parseField :: String -> a

instance {-# OVERLAPPABLE #-} (Read a) => ParseField a where
  parseField = read

instance ParseField String where
  parseField = id

instance (KnownSymbol env, ParseField a, MonadIO m) => DefaultParser m 'Required env a where
  defaultFieldParser = RequiredParser $ do
    env <- liftIO . getEnv . symbolVal $ Proxy @env
    return $ parseField env

instance (KnownSymbol env, ParseField a, MonadIO m) => DefaultParser m 'Optional env a where
  defaultFieldParser = OptionalParser $ do
    env <- liftIO . lookupEnv . symbolVal $ Proxy @env
    return $ parseField <$> env

class HasParser f where
  type ParserC f (m :: * -> *) :: Constraint
  type ParserC f m = MonadIO m

  defaultParser :: ParserC f m => f (Parser m)

  default defaultParser
    :: ( Generic (f (Parser m))
       , GHasParser m (Rep (f (Parser m))) (Rep (f Defaults)) (Rep (f ConfigIdentity))
       )
    => f (Parser m)
  defaultParser = to gDefaultParser

  parse :: (ParserC f m) => f Defaults -> f (Parser m) -> m (f ConfigIdentity)

  default parse
    :: ( Generic (f (Parser m))
       , Generic (f Defaults)
       , Generic (f ConfigIdentity)
       , GHasParser m (Rep (f (Parser m))) (Rep (f Defaults)) (Rep (f ConfigIdentity))
       , ParserC f m
       , Functor m
       )
    => f Defaults -> f (Parser m) -> m (f ConfigIdentity)
  parse defaults parser = to <$> gParse (from parser) (from defaults)

  printDocs :: f Defaults -> IO ()

  default printDocs
    :: ( Generic (f Defaults)
       , GHasParser IO (Rep (f (Parser IO))) (Rep (f Defaults)) (Rep (f ConfigIdentity))
       )
    => f Defaults -> IO ()
  printDocs = gPrintDocs (Proxy @IO) . from


parseDefault :: (HasParser f, ParserC f IO) => f Defaults -> IO (f ConfigIdentity)
parseDefault defaults = parse defaults defaultParser `onException` printDocs defaults

class GHasParser m p d i | p -> d, p -> i, d -> i, d m -> p, p -> m where
  gDefaultParser :: p x

  gParse :: p x -> d x -> m (i x)

  gPrintDocs :: Proxy m -> d x -> IO ()

instance (HasParser nested, ParserC nested m, Monad m) => GHasParser m (K1 x (nested (Parser m))) (K1 x (nested Defaults)) (K1 x (nested ConfigIdentity)) where
  gDefaultParser = K1 defaultParser

  gParse (K1 parser) (K1 defaults) = K1 <$> parse defaults parser

  gPrintDocs _ (K1 defaults) = printDocs defaults

instance (Monad m, KnownSymbol env, Show a, DefaultParser m t env a) => GHasParser m (K1 x (Parser m t env a)) (K1 x (Defaults t env a)) (K1 x a) where
  gDefaultParser = K1 defaultFieldParser

  gParse (K1 (RequiredParser m)) (K1 _) = K1 <$> m
  gParse (K1 (OptionalParser m)) (K1 (Default v)) = K1 . fromMaybe v <$> m

  gPrintDocs _ (K1 NoDefault) = putStrLn $ mconcat
    [symbolVal (Proxy @env)
    ," : required"
    ]
  gPrintDocs _ (K1 (Default v)) = putStrLn $ mconcat
    [symbolVal (Proxy @env)
    ," : optional, default "
    ,show v
    ]

instance (GHasParser m p d i, Monad m) => GHasParser m (M1 a b p) (M1 a b d) (M1 a b i) where
  gDefaultParser = M1 gDefaultParser

  gParse (M1 parser) (M1 defaults) = M1 <$> gParse parser defaults

  gPrintDocs p (M1 defaults) = gPrintDocs p defaults

instance (GHasParser m pl dl il, GHasParser m pr dr ir, Monad m) => GHasParser m (pl :*: pr) (dl :*: dr) (il :*: ir) where
  gDefaultParser = gDefaultParser :*: gDefaultParser

  gParse (lParser :*: rParser) (lDefaults :*: rDefaults) = do
    lResult <- gParse lParser lDefaults
    rResult <- gParse rParser rDefaults
    return $ lResult :*: rResult

  gPrintDocs p (dl :*: dr) = do
    gPrintDocs p dl
    gPrintDocs p dr

type family HKD f (t :: FieldType) (env :: Symbol) a where
  HKD ConfigIdentity t env a = a
  HKD f t env a = f t env a
