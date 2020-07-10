{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsProgramDateTime (
  HlsProgramDateTime (
    ..
    , HPDTExclude
    , HPDTInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
data HlsProgramDateTime = HlsProgramDateTime' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern HPDTExclude :: HlsProgramDateTime
pattern HPDTExclude = HlsProgramDateTime' "EXCLUDE"

pattern HPDTInclude :: HlsProgramDateTime
pattern HPDTInclude = HlsProgramDateTime' "INCLUDE"

{-# COMPLETE
  HPDTExclude,
  HPDTInclude,
  HlsProgramDateTime' #-}

instance FromText HlsProgramDateTime where
    parser = (HlsProgramDateTime' . mk) <$> takeText

instance ToText HlsProgramDateTime where
    toText (HlsProgramDateTime' ci) = original ci

-- | Represents an enum of /known/ $HlsProgramDateTime.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsProgramDateTime where
    toEnum i = case i of
        0 -> HPDTExclude
        1 -> HPDTInclude
        _ -> (error . showText) $ "Unknown index for HlsProgramDateTime: " <> toText i
    fromEnum x = case x of
        HPDTExclude -> 0
        HPDTInclude -> 1
        HlsProgramDateTime' name -> (error . showText) $ "Unknown HlsProgramDateTime: " <> original name

-- | Represents the bounds of /known/ $HlsProgramDateTime.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsProgramDateTime where
    minBound = HPDTExclude
    maxBound = HPDTInclude

instance Hashable     HlsProgramDateTime
instance NFData       HlsProgramDateTime
instance ToByteString HlsProgramDateTime
instance ToQuery      HlsProgramDateTime
instance ToHeader     HlsProgramDateTime

instance ToJSON HlsProgramDateTime where
    toJSON = toJSONText

instance FromJSON HlsProgramDateTime where
    parseJSON = parseJSONText "HlsProgramDateTime"
