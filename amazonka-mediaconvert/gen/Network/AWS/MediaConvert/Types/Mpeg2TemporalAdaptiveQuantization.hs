{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization (
  Mpeg2TemporalAdaptiveQuantization (
    ..
    , MTAQDisabled
    , MTAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data Mpeg2TemporalAdaptiveQuantization = Mpeg2TemporalAdaptiveQuantization' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern MTAQDisabled :: Mpeg2TemporalAdaptiveQuantization
pattern MTAQDisabled = Mpeg2TemporalAdaptiveQuantization' "DISABLED"

pattern MTAQEnabled :: Mpeg2TemporalAdaptiveQuantization
pattern MTAQEnabled = Mpeg2TemporalAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  MTAQDisabled,
  MTAQEnabled,
  Mpeg2TemporalAdaptiveQuantization' #-}

instance FromText Mpeg2TemporalAdaptiveQuantization where
    parser = (Mpeg2TemporalAdaptiveQuantization' . mk) <$> takeText

instance ToText Mpeg2TemporalAdaptiveQuantization where
    toText (Mpeg2TemporalAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2TemporalAdaptiveQuantization where
    toEnum i = case i of
        0 -> MTAQDisabled
        1 -> MTAQEnabled
        _ -> (error . showText) $ "Unknown index for Mpeg2TemporalAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        MTAQDisabled -> 0
        MTAQEnabled -> 1
        Mpeg2TemporalAdaptiveQuantization' name -> (error . showText) $ "Unknown Mpeg2TemporalAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $Mpeg2TemporalAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2TemporalAdaptiveQuantization where
    minBound = MTAQDisabled
    maxBound = MTAQEnabled

instance Hashable     Mpeg2TemporalAdaptiveQuantization
instance NFData       Mpeg2TemporalAdaptiveQuantization
instance ToByteString Mpeg2TemporalAdaptiveQuantization
instance ToQuery      Mpeg2TemporalAdaptiveQuantization
instance ToHeader     Mpeg2TemporalAdaptiveQuantization

instance ToJSON Mpeg2TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2TemporalAdaptiveQuantization"
