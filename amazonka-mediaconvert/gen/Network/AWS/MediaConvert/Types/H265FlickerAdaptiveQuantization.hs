{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization (
  H265FlickerAdaptiveQuantization (
    ..
    , HFAQDisabled
    , HFAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
data H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern HFAQDisabled :: H265FlickerAdaptiveQuantization
pattern HFAQDisabled = H265FlickerAdaptiveQuantization' "DISABLED"

pattern HFAQEnabled :: H265FlickerAdaptiveQuantization
pattern HFAQEnabled = H265FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  HFAQDisabled,
  HFAQEnabled,
  H265FlickerAdaptiveQuantization' #-}

instance FromText H265FlickerAdaptiveQuantization where
    parser = (H265FlickerAdaptiveQuantization' . mk) <$> takeText

instance ToText H265FlickerAdaptiveQuantization where
    toText (H265FlickerAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H265FlickerAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265FlickerAdaptiveQuantization where
    toEnum i = case i of
        0 -> HFAQDisabled
        1 -> HFAQEnabled
        _ -> (error . showText) $ "Unknown index for H265FlickerAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HFAQDisabled -> 0
        HFAQEnabled -> 1
        H265FlickerAdaptiveQuantization' name -> (error . showText) $ "Unknown H265FlickerAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H265FlickerAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265FlickerAdaptiveQuantization where
    minBound = HFAQDisabled
    maxBound = HFAQEnabled

instance Hashable     H265FlickerAdaptiveQuantization
instance NFData       H265FlickerAdaptiveQuantization
instance ToByteString H265FlickerAdaptiveQuantization
instance ToQuery      H265FlickerAdaptiveQuantization
instance ToHeader     H265FlickerAdaptiveQuantization

instance ToJSON H265FlickerAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265FlickerAdaptiveQuantization where
    parseJSON = parseJSONText "H265FlickerAdaptiveQuantization"
