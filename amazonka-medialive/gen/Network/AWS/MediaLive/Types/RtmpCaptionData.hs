{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.RtmpCaptionData (
  RtmpCaptionData (
    ..
    , All
    , FIELD1608
    , FIELD1AndFIELD2608
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Rtmp Caption Data
data RtmpCaptionData = RtmpCaptionData' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern All :: RtmpCaptionData
pattern All = RtmpCaptionData' "ALL"

pattern FIELD1608 :: RtmpCaptionData
pattern FIELD1608 = RtmpCaptionData' "FIELD1_608"

pattern FIELD1AndFIELD2608 :: RtmpCaptionData
pattern FIELD1AndFIELD2608 = RtmpCaptionData' "FIELD1_AND_FIELD2_608"

{-# COMPLETE
  All,
  FIELD1608,
  FIELD1AndFIELD2608,
  RtmpCaptionData' #-}

instance FromText RtmpCaptionData where
    parser = (RtmpCaptionData' . mk) <$> takeText

instance ToText RtmpCaptionData where
    toText (RtmpCaptionData' ci) = original ci

-- | Represents an enum of /known/ $RtmpCaptionData.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RtmpCaptionData where
    toEnum i = case i of
        0 -> All
        1 -> FIELD1608
        2 -> FIELD1AndFIELD2608
        _ -> (error . showText) $ "Unknown index for RtmpCaptionData: " <> toText i
    fromEnum x = case x of
        All -> 0
        FIELD1608 -> 1
        FIELD1AndFIELD2608 -> 2
        RtmpCaptionData' name -> (error . showText) $ "Unknown RtmpCaptionData: " <> original name

-- | Represents the bounds of /known/ $RtmpCaptionData.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RtmpCaptionData where
    minBound = All
    maxBound = FIELD1AndFIELD2608

instance Hashable     RtmpCaptionData
instance NFData       RtmpCaptionData
instance ToByteString RtmpCaptionData
instance ToQuery      RtmpCaptionData
instance ToHeader     RtmpCaptionData

instance ToJSON RtmpCaptionData where
    toJSON = toJSONText

instance FromJSON RtmpCaptionData where
    parseJSON = parseJSONText "RtmpCaptionData"
