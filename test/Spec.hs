{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where
import qualified Data.Aeson as Aeson
import JavaScript.JSValJSON
import Test.Hspec
import Data.Proxy
import Data.Typeable
import qualified Data.ByteString.Lazy as BSL
import qualified Test.QuickCheck as QC
import qualified Data.JSString as JSS
import Data.Foldable (for_)
import Data.Int (Int32)
import qualified Data.Vector as V
import Test.QuickCheck.Instances ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HMS
import Data.JSString.Text

roundtripBackwards :: forall a. (Typeable a, Eq a, Aeson.ToJSON a, FromJSON a, QC.Arbitrary a, Show a) => Proxy a -> SpecWith (Arg QC.Property)
roundtripBackwards p = it (show (typeRep p)) $ QC.property $ \(x :: a) -> QC.ioProperty $ do
  let s = textToJSString (T.decodeUtf8 (BSL.toStrict (Aeson.encode x)))
  mbX <- parseJSONFromString s parseJSON
  case mbX of
    Left err -> fail ("Could not decode: " ++ err)
    Right x' -> return (x == x')

roundtripForwards :: forall a. (Typeable a, Eq a, Aeson.FromJSON a, ToJSON a, QC.Arbitrary a, Show a) => Proxy a -> SpecWith (Arg QC.Property)
roundtripForwards p = it (show (typeRep p)) $ QC.property $ \(x :: a) -> QC.ioProperty $ do
  s <- BSL.fromStrict . T.encodeUtf8 . textFromJSString <$> (toJSONString =<< toJSON x)
  case Aeson.eitherDecode' s of
    Left err -> fail ("Could not decode: " ++ err)
    Right x' -> return (x == x')

data SomeProxy = forall a. (Typeable a, Eq a, Aeson.ToJSON a, ToJSON a, Aeson.FromJSON a, FromJSON a, QC.Arbitrary a, Show a) => SomeProxy (Proxy a)

main :: IO ()
main = do
  hspec $ do
    describe "types roundtrip backwards" $ do
      for_ types (\(SomeProxy p) -> roundtripBackwards p)
    describe "types roundtrip forwards" $ do
      for_ types (\(SomeProxy p) -> roundtripBackwards p)
  where
    types =
      [ SomeProxy (Proxy @())
      , SomeProxy (Proxy @Double)
      , SomeProxy (Proxy @Bool)
      , SomeProxy (Proxy @Int32)
      , SomeProxy (Proxy @Int)
      , SomeProxy (Proxy @Char)
      , SomeProxy (Proxy @(Maybe Double))
      , SomeProxy (Proxy @(V.Vector Double))
      , SomeProxy (Proxy @(Maybe (V.Vector Double)))
      , SomeProxy (Proxy @(V.Vector (Maybe Double)))
      , SomeProxy (Proxy @String)
      , SomeProxy (Proxy @T.Text)
      , SomeProxy (Proxy @(V.Vector String))
      , SomeProxy (Proxy @(V.Vector Char))
      , SomeProxy (Proxy @[Int32])
      , SomeProxy (Proxy @(HMS.HashMap String (Maybe ())))
      , SomeProxy (Proxy @(HMS.HashMap String (Maybe Bool)))
      , SomeProxy (Proxy @(HMS.HashMap T.Text (V.Vector Int32)))
      ]