{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson
import JavaScript.JSValJSON
import qualified JavaScript.JSValJSON.TH as TH (defaultOptions, Options(..), SumEncoding(..))
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
import GHC.Generics (Generic)
import qualified Generic.Random as GA
import qualified Data.Map as M
import Data.Hashable (Hashable)
import Data.Time

import qualified Spec.TH as TH

data Type1 a = Type1
  { type1One :: Int
  , type1Two :: a
  , type1Three :: Maybe Double
  } deriving (Eq, Show, Generic)
instance QC.Arbitrary a => QC.Arbitrary (Type1 a) where arbitrary = GA.genericArbitrary GA.uniform
TH.deriveJSON TH.defaultOptions ''Type1

data Type4 a = Type4
  { type4One :: a
  , type4Two :: Bool
  , type4Three :: Maybe Double
  } deriving (Eq, Show, Generic)
instance QC.Arbitrary a => QC.Arbitrary (Type4 a) where arbitrary = GA.genericArbitrary GA.uniform
TH.deriveJSON
  TH.defaultOptions{TH.omitNothingFields = True}
  ''Type4

data Type2
  = Type2Foo (Type1 ())
  | Type2Bar
  | Type2Baz Int Bool
  deriving (Eq, Show, Generic)
instance QC.Arbitrary Type2 where arbitrary = GA.genericArbitrary GA.uniform
TH.deriveJSON TH.defaultOptions ''Type2

data Type3 a
  = Type3Foo (Type1 (Either Int Bool))
  | Type3Bar
  | Type3Baz a Bool
  deriving (Eq, Show, Generic)
instance QC.Arbitrary a => QC.Arbitrary (Type3 a) where arbitrary = GA.genericArbitrary GA.uniform
TH.deriveJSON
  TH.defaultOptions{TH.allNullaryToStringTag = False}
  ''Type3

data Type5
  = Type5Foo (Type1 (M.Map Char Int))
  | Type5Bar
  | Type5Baz Int Bool
  deriving (Eq, Show, Generic)
instance QC.Arbitrary Type5 where arbitrary = GA.genericArbitrary GA.uniform
TH.deriveJSON
  TH.defaultOptions{TH.sumEncoding = TH.ObjectWithSingleField, TH.tagSingleConstructors = True}
  ''Type5

data Type6
  = Type6Foo
  | Type6Bar
  | Type6Baz
  deriving (Eq, Show, Generic)
instance QC.Arbitrary Type6 where arbitrary = GA.genericArbitrary GA.uniform
instance Hashable Type6
TH.deriveJSON
  TH.defaultOptions
    { TH.allNullaryToStringTag = True
    , TH.constructorTagModifier = drop 5
    , TH.sumEncoding = TH.ObjectWithSingleField
    , TH.unwrapUnaryRecords = False
    }
  ''Type6
instance FromJSONKey Type6 where
  parseJSONKey txt = parseJSON (_String txt)
instance ToJSONKey Type6 where
  toJSONKey v = do
    mb <- runParser (withString "toJSONKey string" return) =<< toJSON v
    case mb of
      Left err -> fail err
      Right x -> return x

instance Aeson.FromJSONKey Type6 where
  fromJSONKey = Aeson.FromJSONKeyTextParser (\txt -> Aeson.parseJSON (Aeson.String txt))
instance Aeson.ToJSONKey Type6 where
  toJSONKey = Aeson.toJSONKeyText f
    where
      f x = case Aeson.toJSON x of
        Aeson.String txt -> txt
        _ -> error "toJSONKey: not text"

roundtripBackwards :: forall a. (Typeable a, Eq a, Aeson.ToJSON a, FromJSON a, QC.Arbitrary a, Show a) => a -> QC.Property
roundtripBackwards x = QC.ioProperty $ do
  let s = textToJSString (T.decodeUtf8 (BSL.toStrict (Aeson.encode x)))
  mbX <- parseJSONFromString s parseJSON
  case mbX of
    Left err -> fail ("Could not decode: " ++ err)
    Right x' -> return (x QC.=== x')

roundtripForwards :: forall a. (Typeable a, Eq a, Aeson.FromJSON a, ToJSON a, QC.Arbitrary a, Show a) => a -> QC.Property
roundtripForwards x = QC.ioProperty $ do
  s <- BSL.fromStrict . T.encodeUtf8 . textFromJSString <$> (toJSONString =<< toJSON x)
  case Aeson.eitherDecode' s of
    Left err -> fail ("Could not decode: " ++ err)
    Right x' -> return (x QC.=== x')

roundtripBackwardsQC :: forall a. (Typeable a, Eq a, Aeson.ToJSON a, FromJSON a, QC.Arbitrary a, Show a) => Proxy a -> SpecWith (Arg QC.Property)
roundtripBackwardsQC p = it (show (typeRep p)) (QC.property (\(x :: a) -> roundtripBackwards x))

roundtripForwardsQC :: forall a. (Typeable a, Eq a, Aeson.FromJSON a, ToJSON a, QC.Arbitrary a, Show a) => Proxy a -> SpecWith (Arg QC.Property)
roundtripForwardsQC p = it (show (typeRep p)) (QC.property (\(x :: a) -> roundtripForwards x))

data SomeProxy = forall a. (Typeable a, Eq a, Aeson.ToJSON a, ToJSON a, Aeson.FromJSON a, FromJSON a, QC.Arbitrary a, Show a) => SomeProxy (Proxy a)

main :: IO ()
main = do
  hspec $ do
    describe "types roundtrip backwards" $ do
      for_ types (\(SomeProxy p) -> roundtripBackwardsQC p)
    describe "types roundtrip forwards" $ do
      for_ types (\(SomeProxy p) -> roundtripForwardsQC p)
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
      , SomeProxy (Proxy @(Either (V.Vector Bool) Double))
      , SomeProxy (Proxy @(V.Vector (Either Bool Double)))
      , SomeProxy (Proxy @(Type1 String))
      , SomeProxy (Proxy @Type2)
      , SomeProxy (Proxy @(Type3 Bool))
      , SomeProxy (Proxy @(Type4 (Maybe Int)))
      , SomeProxy (Proxy @Type5)
      , SomeProxy (Proxy @(V.Vector (Type1 Double)))
      , SomeProxy (Proxy @Type6)
      , SomeProxy (Proxy @(HMS.HashMap Type6 Int))
      , SomeProxy (Proxy @Day)
      ]
