{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkS3BackupMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SplunkS3BackupMode (
  SplunkS3BackupMode (
    ..
    , AllEvents
    , FailedEventsOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SplunkS3BackupMode = SplunkS3BackupMode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AllEvents :: SplunkS3BackupMode
pattern AllEvents = SplunkS3BackupMode' "AllEvents"

pattern FailedEventsOnly :: SplunkS3BackupMode
pattern FailedEventsOnly = SplunkS3BackupMode' "FailedEventsOnly"

{-# COMPLETE
  AllEvents,
  FailedEventsOnly,
  SplunkS3BackupMode' #-}

instance FromText SplunkS3BackupMode where
    parser = (SplunkS3BackupMode' . mk) <$> takeText

instance ToText SplunkS3BackupMode where
    toText (SplunkS3BackupMode' ci) = original ci

-- | Represents an enum of /known/ $SplunkS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SplunkS3BackupMode where
    toEnum i = case i of
        0 -> AllEvents
        1 -> FailedEventsOnly
        _ -> (error . showText) $ "Unknown index for SplunkS3BackupMode: " <> toText i
    fromEnum x = case x of
        AllEvents -> 0
        FailedEventsOnly -> 1
        SplunkS3BackupMode' name -> (error . showText) $ "Unknown SplunkS3BackupMode: " <> original name

-- | Represents the bounds of /known/ $SplunkS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SplunkS3BackupMode where
    minBound = AllEvents
    maxBound = FailedEventsOnly

instance Hashable     SplunkS3BackupMode
instance NFData       SplunkS3BackupMode
instance ToByteString SplunkS3BackupMode
instance ToQuery      SplunkS3BackupMode
instance ToHeader     SplunkS3BackupMode

instance ToJSON SplunkS3BackupMode where
    toJSON = toJSONText

instance FromJSON SplunkS3BackupMode where
    parseJSON = parseJSONText "SplunkS3BackupMode"
