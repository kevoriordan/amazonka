{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacInputType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacInputType (
  AacInputType (
    ..
    , BroadcasterMixedAd
    , Normal
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Aac Input Type
data AacInputType = AacInputType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern BroadcasterMixedAd :: AacInputType
pattern BroadcasterMixedAd = AacInputType' "BROADCASTER_MIXED_AD"

pattern Normal :: AacInputType
pattern Normal = AacInputType' "NORMAL"

{-# COMPLETE
  BroadcasterMixedAd,
  Normal,
  AacInputType' #-}

instance FromText AacInputType where
    parser = (AacInputType' . mk) <$> takeText

instance ToText AacInputType where
    toText (AacInputType' ci) = original ci

-- | Represents an enum of /known/ $AacInputType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacInputType where
    toEnum i = case i of
        0 -> BroadcasterMixedAd
        1 -> Normal
        _ -> (error . showText) $ "Unknown index for AacInputType: " <> toText i
    fromEnum x = case x of
        BroadcasterMixedAd -> 0
        Normal -> 1
        AacInputType' name -> (error . showText) $ "Unknown AacInputType: " <> original name

-- | Represents the bounds of /known/ $AacInputType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacInputType where
    minBound = BroadcasterMixedAd
    maxBound = Normal

instance Hashable     AacInputType
instance NFData       AacInputType
instance ToByteString AacInputType
instance ToQuery      AacInputType
instance ToHeader     AacInputType

instance ToJSON AacInputType where
    toJSON = toJSONText

instance FromJSON AacInputType where
    parseJSON = parseJSONText "AacInputType"
