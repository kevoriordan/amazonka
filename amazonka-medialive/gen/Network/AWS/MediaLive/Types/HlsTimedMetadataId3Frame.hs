{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame (
  HlsTimedMetadataId3Frame (
    ..
    , HTMIFNone
    , HTMIFPriv
    , HTMIFTdrl
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Timed Metadata Id3 Frame
data HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HTMIFNone :: HlsTimedMetadataId3Frame
pattern HTMIFNone = HlsTimedMetadataId3Frame' "NONE"

pattern HTMIFPriv :: HlsTimedMetadataId3Frame
pattern HTMIFPriv = HlsTimedMetadataId3Frame' "PRIV"

pattern HTMIFTdrl :: HlsTimedMetadataId3Frame
pattern HTMIFTdrl = HlsTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  HTMIFNone,
  HTMIFPriv,
  HTMIFTdrl,
  HlsTimedMetadataId3Frame' #-}

instance FromText HlsTimedMetadataId3Frame where
    parser = (HlsTimedMetadataId3Frame' . mk) <$> takeText

instance ToText HlsTimedMetadataId3Frame where
    toText (HlsTimedMetadataId3Frame' ci) = original ci

-- | Represents an enum of /known/ $HlsTimedMetadataId3Frame.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsTimedMetadataId3Frame where
    toEnum i = case i of
        0 -> HTMIFNone
        1 -> HTMIFPriv
        2 -> HTMIFTdrl
        _ -> (error . showText) $ "Unknown index for HlsTimedMetadataId3Frame: " <> toText i
    fromEnum x = case x of
        HTMIFNone -> 0
        HTMIFPriv -> 1
        HTMIFTdrl -> 2
        HlsTimedMetadataId3Frame' name -> (error . showText) $ "Unknown HlsTimedMetadataId3Frame: " <> original name

-- | Represents the bounds of /known/ $HlsTimedMetadataId3Frame.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsTimedMetadataId3Frame where
    minBound = HTMIFNone
    maxBound = HTMIFTdrl

instance Hashable     HlsTimedMetadataId3Frame
instance NFData       HlsTimedMetadataId3Frame
instance ToByteString HlsTimedMetadataId3Frame
instance ToQuery      HlsTimedMetadataId3Frame
instance ToHeader     HlsTimedMetadataId3Frame

instance ToJSON HlsTimedMetadataId3Frame where
    toJSON = toJSONText

instance FromJSON HlsTimedMetadataId3Frame where
    parseJSON = parseJSONText "HlsTimedMetadataId3Frame"
