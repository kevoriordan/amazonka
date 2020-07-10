{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantPropertyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.VariantPropertyType (
  VariantPropertyType (
    ..
    , DataCaptureConfig
    , DesiredInstanceCount
    , DesiredWeight
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VariantPropertyType = VariantPropertyType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern DataCaptureConfig :: VariantPropertyType
pattern DataCaptureConfig = VariantPropertyType' "DataCaptureConfig"

pattern DesiredInstanceCount :: VariantPropertyType
pattern DesiredInstanceCount = VariantPropertyType' "DesiredInstanceCount"

pattern DesiredWeight :: VariantPropertyType
pattern DesiredWeight = VariantPropertyType' "DesiredWeight"

{-# COMPLETE
  DataCaptureConfig,
  DesiredInstanceCount,
  DesiredWeight,
  VariantPropertyType' #-}

instance FromText VariantPropertyType where
    parser = (VariantPropertyType' . mk) <$> takeText

instance ToText VariantPropertyType where
    toText (VariantPropertyType' ci) = original ci

-- | Represents an enum of /known/ $VariantPropertyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VariantPropertyType where
    toEnum i = case i of
        0 -> DataCaptureConfig
        1 -> DesiredInstanceCount
        2 -> DesiredWeight
        _ -> (error . showText) $ "Unknown index for VariantPropertyType: " <> toText i
    fromEnum x = case x of
        DataCaptureConfig -> 0
        DesiredInstanceCount -> 1
        DesiredWeight -> 2
        VariantPropertyType' name -> (error . showText) $ "Unknown VariantPropertyType: " <> original name

-- | Represents the bounds of /known/ $VariantPropertyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VariantPropertyType where
    minBound = DataCaptureConfig
    maxBound = DesiredWeight

instance Hashable     VariantPropertyType
instance NFData       VariantPropertyType
instance ToByteString VariantPropertyType
instance ToQuery      VariantPropertyType
instance ToHeader     VariantPropertyType

instance ToJSON VariantPropertyType where
    toJSON = toJSONText
