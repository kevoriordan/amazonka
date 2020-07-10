{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization (
  H264TemporalAdaptiveQuantization (
    ..
    , H26Disabled
    , H26Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data H264TemporalAdaptiveQuantization = H264TemporalAdaptiveQuantization' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern H26Disabled :: H264TemporalAdaptiveQuantization
pattern H26Disabled = H264TemporalAdaptiveQuantization' "DISABLED"

pattern H26Enabled :: H264TemporalAdaptiveQuantization
pattern H26Enabled = H264TemporalAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H26Disabled,
  H26Enabled,
  H264TemporalAdaptiveQuantization' #-}

instance FromText H264TemporalAdaptiveQuantization where
    parser = (H264TemporalAdaptiveQuantization' . mk) <$> takeText

instance ToText H264TemporalAdaptiveQuantization where
    toText (H264TemporalAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H264TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264TemporalAdaptiveQuantization where
    toEnum i = case i of
        0 -> H26Disabled
        1 -> H26Enabled
        _ -> (error . showText) $ "Unknown index for H264TemporalAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        H26Disabled -> 0
        H26Enabled -> 1
        H264TemporalAdaptiveQuantization' name -> (error . showText) $ "Unknown H264TemporalAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H264TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264TemporalAdaptiveQuantization where
    minBound = H26Disabled
    maxBound = H26Enabled

instance Hashable     H264TemporalAdaptiveQuantization
instance NFData       H264TemporalAdaptiveQuantization
instance ToByteString H264TemporalAdaptiveQuantization
instance ToQuery      H264TemporalAdaptiveQuantization
instance ToHeader     H264TemporalAdaptiveQuantization

instance ToJSON H264TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "H264TemporalAdaptiveQuantization"
