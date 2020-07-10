{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLS3DataType (
  AutoMLS3DataType (
    ..
    , AMLSDTManifestFile
    , AMLSDTS3Prefix
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLS3DataType = AutoMLS3DataType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AMLSDTManifestFile :: AutoMLS3DataType
pattern AMLSDTManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AMLSDTS3Prefix :: AutoMLS3DataType
pattern AMLSDTS3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE
  AMLSDTManifestFile,
  AMLSDTS3Prefix,
  AutoMLS3DataType' #-}

instance FromText AutoMLS3DataType where
    parser = (AutoMLS3DataType' . mk) <$> takeText

instance ToText AutoMLS3DataType where
    toText (AutoMLS3DataType' ci) = original ci

-- | Represents an enum of /known/ $AutoMLS3DataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLS3DataType where
    toEnum i = case i of
        0 -> AMLSDTManifestFile
        1 -> AMLSDTS3Prefix
        _ -> (error . showText) $ "Unknown index for AutoMLS3DataType: " <> toText i
    fromEnum x = case x of
        AMLSDTManifestFile -> 0
        AMLSDTS3Prefix -> 1
        AutoMLS3DataType' name -> (error . showText) $ "Unknown AutoMLS3DataType: " <> original name

-- | Represents the bounds of /known/ $AutoMLS3DataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLS3DataType where
    minBound = AMLSDTManifestFile
    maxBound = AMLSDTS3Prefix

instance Hashable     AutoMLS3DataType
instance NFData       AutoMLS3DataType
instance ToByteString AutoMLS3DataType
instance ToQuery      AutoMLS3DataType
instance ToHeader     AutoMLS3DataType

instance ToJSON AutoMLS3DataType where
    toJSON = toJSONText

instance FromJSON AutoMLS3DataType where
    parseJSON = parseJSONText "AutoMLS3DataType"
