{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncS3Format
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncS3Format (
  ResourceDataSyncS3Format (
    ..
    , JSONSerDe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceDataSyncS3Format = ResourceDataSyncS3Format' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern JSONSerDe :: ResourceDataSyncS3Format
pattern JSONSerDe = ResourceDataSyncS3Format' "JsonSerDe"

{-# COMPLETE
  JSONSerDe,
  ResourceDataSyncS3Format' #-}

instance FromText ResourceDataSyncS3Format where
    parser = (ResourceDataSyncS3Format' . mk) <$> takeText

instance ToText ResourceDataSyncS3Format where
    toText (ResourceDataSyncS3Format' ci) = original ci

-- | Represents an enum of /known/ $ResourceDataSyncS3Format.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceDataSyncS3Format where
    toEnum i = case i of
        0 -> JSONSerDe
        _ -> (error . showText) $ "Unknown index for ResourceDataSyncS3Format: " <> toText i
    fromEnum x = case x of
        JSONSerDe -> 0
        ResourceDataSyncS3Format' name -> (error . showText) $ "Unknown ResourceDataSyncS3Format: " <> original name

-- | Represents the bounds of /known/ $ResourceDataSyncS3Format.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceDataSyncS3Format where
    minBound = JSONSerDe
    maxBound = JSONSerDe

instance Hashable     ResourceDataSyncS3Format
instance NFData       ResourceDataSyncS3Format
instance ToByteString ResourceDataSyncS3Format
instance ToQuery      ResourceDataSyncS3Format
instance ToHeader     ResourceDataSyncS3Format

instance ToJSON ResourceDataSyncS3Format where
    toJSON = toJSONText

instance FromJSON ResourceDataSyncS3Format where
    parseJSON = parseJSONText "ResourceDataSyncS3Format"
