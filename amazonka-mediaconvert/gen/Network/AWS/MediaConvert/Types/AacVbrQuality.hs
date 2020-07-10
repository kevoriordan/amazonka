{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacVbrQuality
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacVbrQuality (
  AacVbrQuality (
    ..
    , AVQHigh
    , AVQLow
    , AVQMediumHigh
    , AVQMediumLow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
data AacVbrQuality = AacVbrQuality' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern AVQHigh :: AacVbrQuality
pattern AVQHigh = AacVbrQuality' "HIGH"

pattern AVQLow :: AacVbrQuality
pattern AVQLow = AacVbrQuality' "LOW"

pattern AVQMediumHigh :: AacVbrQuality
pattern AVQMediumHigh = AacVbrQuality' "MEDIUM_HIGH"

pattern AVQMediumLow :: AacVbrQuality
pattern AVQMediumLow = AacVbrQuality' "MEDIUM_LOW"

{-# COMPLETE
  AVQHigh,
  AVQLow,
  AVQMediumHigh,
  AVQMediumLow,
  AacVbrQuality' #-}

instance FromText AacVbrQuality where
    parser = (AacVbrQuality' . mk) <$> takeText

instance ToText AacVbrQuality where
    toText (AacVbrQuality' ci) = original ci

-- | Represents an enum of /known/ $AacVbrQuality.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacVbrQuality where
    toEnum i = case i of
        0 -> AVQHigh
        1 -> AVQLow
        2 -> AVQMediumHigh
        3 -> AVQMediumLow
        _ -> (error . showText) $ "Unknown index for AacVbrQuality: " <> toText i
    fromEnum x = case x of
        AVQHigh -> 0
        AVQLow -> 1
        AVQMediumHigh -> 2
        AVQMediumLow -> 3
        AacVbrQuality' name -> (error . showText) $ "Unknown AacVbrQuality: " <> original name

-- | Represents the bounds of /known/ $AacVbrQuality.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacVbrQuality where
    minBound = AVQHigh
    maxBound = AVQMediumLow

instance Hashable     AacVbrQuality
instance NFData       AacVbrQuality
instance ToByteString AacVbrQuality
instance ToQuery      AacVbrQuality
instance ToHeader     AacVbrQuality

instance ToJSON AacVbrQuality where
    toJSON = toJSONText

instance FromJSON AacVbrQuality where
    parseJSON = parseJSONText "AacVbrQuality"
