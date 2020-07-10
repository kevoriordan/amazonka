{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MinimumProtocolVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.MinimumProtocolVersion (
  MinimumProtocolVersion (
    ..
    , MPVSSLV3
    , MPVTLSV1
    , MPVTLSV12016
    , MPVTLSV1_12016
    , MPVTLSV1_22018
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MinimumProtocolVersion = MinimumProtocolVersion' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern MPVSSLV3 :: MinimumProtocolVersion
pattern MPVSSLV3 = MinimumProtocolVersion' "SSLv3"

pattern MPVTLSV1 :: MinimumProtocolVersion
pattern MPVTLSV1 = MinimumProtocolVersion' "TLSv1"

pattern MPVTLSV12016 :: MinimumProtocolVersion
pattern MPVTLSV12016 = MinimumProtocolVersion' "TLSv1_2016"

pattern MPVTLSV1_12016 :: MinimumProtocolVersion
pattern MPVTLSV1_12016 = MinimumProtocolVersion' "TLSv1.1_2016"

pattern MPVTLSV1_22018 :: MinimumProtocolVersion
pattern MPVTLSV1_22018 = MinimumProtocolVersion' "TLSv1.2_2018"

{-# COMPLETE
  MPVSSLV3,
  MPVTLSV1,
  MPVTLSV12016,
  MPVTLSV1_12016,
  MPVTLSV1_22018,
  MinimumProtocolVersion' #-}

instance FromText MinimumProtocolVersion where
    parser = (MinimumProtocolVersion' . mk) <$> takeText

instance ToText MinimumProtocolVersion where
    toText (MinimumProtocolVersion' ci) = original ci

-- | Represents an enum of /known/ $MinimumProtocolVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MinimumProtocolVersion where
    toEnum i = case i of
        0 -> MPVSSLV3
        1 -> MPVTLSV1
        2 -> MPVTLSV12016
        3 -> MPVTLSV1_12016
        4 -> MPVTLSV1_22018
        _ -> (error . showText) $ "Unknown index for MinimumProtocolVersion: " <> toText i
    fromEnum x = case x of
        MPVSSLV3 -> 0
        MPVTLSV1 -> 1
        MPVTLSV12016 -> 2
        MPVTLSV1_12016 -> 3
        MPVTLSV1_22018 -> 4
        MinimumProtocolVersion' name -> (error . showText) $ "Unknown MinimumProtocolVersion: " <> original name

-- | Represents the bounds of /known/ $MinimumProtocolVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MinimumProtocolVersion where
    minBound = MPVSSLV3
    maxBound = MPVTLSV1_22018

instance Hashable     MinimumProtocolVersion
instance NFData       MinimumProtocolVersion
instance ToByteString MinimumProtocolVersion
instance ToQuery      MinimumProtocolVersion
instance ToHeader     MinimumProtocolVersion

instance FromXML MinimumProtocolVersion where
    parseXML = parseXMLText "MinimumProtocolVersion"

instance ToXML MinimumProtocolVersion where
    toXML = toXMLText
