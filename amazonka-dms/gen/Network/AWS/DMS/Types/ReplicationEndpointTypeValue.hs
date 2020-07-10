{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationEndpointTypeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationEndpointTypeValue (
  ReplicationEndpointTypeValue (
    ..
    , Source
    , Target
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicationEndpointTypeValue = ReplicationEndpointTypeValue' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Source :: ReplicationEndpointTypeValue
pattern Source = ReplicationEndpointTypeValue' "source"

pattern Target :: ReplicationEndpointTypeValue
pattern Target = ReplicationEndpointTypeValue' "target"

{-# COMPLETE
  Source,
  Target,
  ReplicationEndpointTypeValue' #-}

instance FromText ReplicationEndpointTypeValue where
    parser = (ReplicationEndpointTypeValue' . mk) <$> takeText

instance ToText ReplicationEndpointTypeValue where
    toText (ReplicationEndpointTypeValue' ci) = original ci

-- | Represents an enum of /known/ $ReplicationEndpointTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationEndpointTypeValue where
    toEnum i = case i of
        0 -> Source
        1 -> Target
        _ -> (error . showText) $ "Unknown index for ReplicationEndpointTypeValue: " <> toText i
    fromEnum x = case x of
        Source -> 0
        Target -> 1
        ReplicationEndpointTypeValue' name -> (error . showText) $ "Unknown ReplicationEndpointTypeValue: " <> original name

-- | Represents the bounds of /known/ $ReplicationEndpointTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationEndpointTypeValue where
    minBound = Source
    maxBound = Target

instance Hashable     ReplicationEndpointTypeValue
instance NFData       ReplicationEndpointTypeValue
instance ToByteString ReplicationEndpointTypeValue
instance ToQuery      ReplicationEndpointTypeValue
instance ToHeader     ReplicationEndpointTypeValue

instance ToJSON ReplicationEndpointTypeValue where
    toJSON = toJSONText

instance FromJSON ReplicationEndpointTypeValue where
    parseJSON = parseJSONText "ReplicationEndpointTypeValue"
