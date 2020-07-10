{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame (
  UdpTimedMetadataId3Frame (
    ..
    , UTMIFNone
    , UTMIFPriv
    , UTMIFTdrl
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Udp Timed Metadata Id3 Frame
data UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern UTMIFNone :: UdpTimedMetadataId3Frame
pattern UTMIFNone = UdpTimedMetadataId3Frame' "NONE"

pattern UTMIFPriv :: UdpTimedMetadataId3Frame
pattern UTMIFPriv = UdpTimedMetadataId3Frame' "PRIV"

pattern UTMIFTdrl :: UdpTimedMetadataId3Frame
pattern UTMIFTdrl = UdpTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  UTMIFNone,
  UTMIFPriv,
  UTMIFTdrl,
  UdpTimedMetadataId3Frame' #-}

instance FromText UdpTimedMetadataId3Frame where
    parser = (UdpTimedMetadataId3Frame' . mk) <$> takeText

instance ToText UdpTimedMetadataId3Frame where
    toText (UdpTimedMetadataId3Frame' ci) = original ci

-- | Represents an enum of /known/ $UdpTimedMetadataId3Frame.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UdpTimedMetadataId3Frame where
    toEnum i = case i of
        0 -> UTMIFNone
        1 -> UTMIFPriv
        2 -> UTMIFTdrl
        _ -> (error . showText) $ "Unknown index for UdpTimedMetadataId3Frame: " <> toText i
    fromEnum x = case x of
        UTMIFNone -> 0
        UTMIFPriv -> 1
        UTMIFTdrl -> 2
        UdpTimedMetadataId3Frame' name -> (error . showText) $ "Unknown UdpTimedMetadataId3Frame: " <> original name

-- | Represents the bounds of /known/ $UdpTimedMetadataId3Frame.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UdpTimedMetadataId3Frame where
    minBound = UTMIFNone
    maxBound = UTMIFTdrl

instance Hashable     UdpTimedMetadataId3Frame
instance NFData       UdpTimedMetadataId3Frame
instance ToByteString UdpTimedMetadataId3Frame
instance ToQuery      UdpTimedMetadataId3Frame
instance ToHeader     UdpTimedMetadataId3Frame

instance ToJSON UdpTimedMetadataId3Frame where
    toJSON = toJSONText

instance FromJSON UdpTimedMetadataId3Frame where
    parseJSON = parseJSONText "UdpTimedMetadataId3Frame"
