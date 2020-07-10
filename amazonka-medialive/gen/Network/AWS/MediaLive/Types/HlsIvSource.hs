{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsIvSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsIvSource (
  HlsIvSource (
    ..
    , Explicit
    , FollowsSegmentNumber
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Iv Source
data HlsIvSource = HlsIvSource' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Explicit :: HlsIvSource
pattern Explicit = HlsIvSource' "EXPLICIT"

pattern FollowsSegmentNumber :: HlsIvSource
pattern FollowsSegmentNumber = HlsIvSource' "FOLLOWS_SEGMENT_NUMBER"

{-# COMPLETE
  Explicit,
  FollowsSegmentNumber,
  HlsIvSource' #-}

instance FromText HlsIvSource where
    parser = (HlsIvSource' . mk) <$> takeText

instance ToText HlsIvSource where
    toText (HlsIvSource' ci) = original ci

-- | Represents an enum of /known/ $HlsIvSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsIvSource where
    toEnum i = case i of
        0 -> Explicit
        1 -> FollowsSegmentNumber
        _ -> (error . showText) $ "Unknown index for HlsIvSource: " <> toText i
    fromEnum x = case x of
        Explicit -> 0
        FollowsSegmentNumber -> 1
        HlsIvSource' name -> (error . showText) $ "Unknown HlsIvSource: " <> original name

-- | Represents the bounds of /known/ $HlsIvSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsIvSource where
    minBound = Explicit
    maxBound = FollowsSegmentNumber

instance Hashable     HlsIvSource
instance NFData       HlsIvSource
instance ToByteString HlsIvSource
instance ToQuery      HlsIvSource
instance ToHeader     HlsIvSource

instance ToJSON HlsIvSource where
    toJSON = toJSONText

instance FromJSON HlsIvSource where
    parseJSON = parseJSONText "HlsIvSource"
