{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitlingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitlingType (
  DvbSubtitlingType (
    ..
    , HearingImpaired
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
data DvbSubtitlingType = DvbSubtitlingType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern HearingImpaired :: DvbSubtitlingType
pattern HearingImpaired = DvbSubtitlingType' "HEARING_IMPAIRED"

pattern Standard :: DvbSubtitlingType
pattern Standard = DvbSubtitlingType' "STANDARD"

{-# COMPLETE
  HearingImpaired,
  Standard,
  DvbSubtitlingType' #-}

instance FromText DvbSubtitlingType where
    parser = (DvbSubtitlingType' . mk) <$> takeText

instance ToText DvbSubtitlingType where
    toText (DvbSubtitlingType' ci) = original ci

-- | Represents an enum of /known/ $DvbSubtitlingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubtitlingType where
    toEnum i = case i of
        0 -> HearingImpaired
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for DvbSubtitlingType: " <> toText i
    fromEnum x = case x of
        HearingImpaired -> 0
        Standard -> 1
        DvbSubtitlingType' name -> (error . showText) $ "Unknown DvbSubtitlingType: " <> original name

-- | Represents the bounds of /known/ $DvbSubtitlingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubtitlingType where
    minBound = HearingImpaired
    maxBound = Standard

instance Hashable     DvbSubtitlingType
instance NFData       DvbSubtitlingType
instance ToByteString DvbSubtitlingType
instance ToQuery      DvbSubtitlingType
instance ToHeader     DvbSubtitlingType

instance ToJSON DvbSubtitlingType where
    toJSON = toJSONText

instance FromJSON DvbSubtitlingType where
    parseJSON = parseJSONText "DvbSubtitlingType"
