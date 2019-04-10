{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Spec.TH (deriveJSON) where

import qualified Data.Aeson.TH as Aeson
import qualified Language.Haskell.TH as TH
import Control.Monad (liftM2)
import qualified JavaScript.JSValJSON.TH as JS

toAesonOptions :: JS.Options -> Aeson.Options
toAesonOptions JS.Options{..} = Aeson.defaultOptions
  { Aeson.sumEncoding = toAesonSumEncoding sumEncoding
  , Aeson.fieldLabelModifier = fieldLabelModifier
  , Aeson.constructorTagModifier = constructorTagModifier
  , Aeson.allNullaryToStringTag = allNullaryToStringTag
  , Aeson.omitNothingFields = omitNothingFields
  , Aeson.unwrapUnaryRecords = unwrapUnaryRecords
  , Aeson.tagSingleConstructors = tagSingleConstructors
  }
  where
    toAesonSumEncoding = \case
      JS.TaggedObject{..} -> Aeson.TaggedObject{..}
      JS.ObjectWithSingleField -> Aeson.ObjectWithSingleField

deriveJSON :: JS.Options -> TH.Name -> TH.Q [TH.Dec]
deriveJSON opts name = fmap concat $ sequenceA
  [ JS.deriveJSON opts name
  , Aeson.deriveJSON (toAesonOptions opts) name
  ]

