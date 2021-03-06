{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265TemporalIds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265TemporalIds (
  H265TemporalIds (
    ..
    , HTIDisabled
    , HTIEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
data H265TemporalIds = H265TemporalIds' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern HTIDisabled :: H265TemporalIds
pattern HTIDisabled = H265TemporalIds' "DISABLED"

pattern HTIEnabled :: H265TemporalIds
pattern HTIEnabled = H265TemporalIds' "ENABLED"

{-# COMPLETE
  HTIDisabled,
  HTIEnabled,
  H265TemporalIds' #-}

instance FromText H265TemporalIds where
    parser = (H265TemporalIds' . mk) <$> takeText

instance ToText H265TemporalIds where
    toText (H265TemporalIds' ci) = original ci

-- | Represents an enum of /known/ $H265TemporalIds.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265TemporalIds where
    toEnum i = case i of
        0 -> HTIDisabled
        1 -> HTIEnabled
        _ -> (error . showText) $ "Unknown index for H265TemporalIds: " <> toText i
    fromEnum x = case x of
        HTIDisabled -> 0
        HTIEnabled -> 1
        H265TemporalIds' name -> (error . showText) $ "Unknown H265TemporalIds: " <> original name

-- | Represents the bounds of /known/ $H265TemporalIds.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265TemporalIds where
    minBound = HTIDisabled
    maxBound = HTIEnabled

instance Hashable     H265TemporalIds
instance NFData       H265TemporalIds
instance ToByteString H265TemporalIds
instance ToQuery      H265TemporalIds
instance ToHeader     H265TemporalIds

instance ToJSON H265TemporalIds where
    toJSON = toJSONText

instance FromJSON H265TemporalIds where
    parseJSON = parseJSONText "H265TemporalIds"
