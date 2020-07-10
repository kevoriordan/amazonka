{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization (
  H265TemporalAdaptiveQuantization (
    ..
    , HTAQDisabled
    , HTAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data H265TemporalAdaptiveQuantization = H265TemporalAdaptiveQuantization' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern HTAQDisabled :: H265TemporalAdaptiveQuantization
pattern HTAQDisabled = H265TemporalAdaptiveQuantization' "DISABLED"

pattern HTAQEnabled :: H265TemporalAdaptiveQuantization
pattern HTAQEnabled = H265TemporalAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  HTAQDisabled,
  HTAQEnabled,
  H265TemporalAdaptiveQuantization' #-}

instance FromText H265TemporalAdaptiveQuantization where
    parser = (H265TemporalAdaptiveQuantization' . mk) <$> takeText

instance ToText H265TemporalAdaptiveQuantization where
    toText (H265TemporalAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H265TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265TemporalAdaptiveQuantization where
    toEnum i = case i of
        0 -> HTAQDisabled
        1 -> HTAQEnabled
        _ -> (error . showText) $ "Unknown index for H265TemporalAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HTAQDisabled -> 0
        HTAQEnabled -> 1
        H265TemporalAdaptiveQuantization' name -> (error . showText) $ "Unknown H265TemporalAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H265TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265TemporalAdaptiveQuantization where
    minBound = HTAQDisabled
    maxBound = HTAQEnabled

instance Hashable     H265TemporalAdaptiveQuantization
instance NFData       H265TemporalAdaptiveQuantization
instance ToByteString H265TemporalAdaptiveQuantization
instance ToQuery      H265TemporalAdaptiveQuantization
instance ToHeader     H265TemporalAdaptiveQuantization

instance ToJSON H265TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "H265TemporalAdaptiveQuantization"
