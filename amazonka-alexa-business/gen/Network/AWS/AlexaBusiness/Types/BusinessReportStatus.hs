{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportStatus (
  BusinessReportStatus (
    ..
    , BRSFailed
    , BRSRunning
    , BRSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BusinessReportStatus = BusinessReportStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern BRSFailed :: BusinessReportStatus
pattern BRSFailed = BusinessReportStatus' "FAILED"

pattern BRSRunning :: BusinessReportStatus
pattern BRSRunning = BusinessReportStatus' "RUNNING"

pattern BRSSucceeded :: BusinessReportStatus
pattern BRSSucceeded = BusinessReportStatus' "SUCCEEDED"

{-# COMPLETE
  BRSFailed,
  BRSRunning,
  BRSSucceeded,
  BusinessReportStatus' #-}

instance FromText BusinessReportStatus where
    parser = (BusinessReportStatus' . mk) <$> takeText

instance ToText BusinessReportStatus where
    toText (BusinessReportStatus' ci) = original ci

-- | Represents an enum of /known/ $BusinessReportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BusinessReportStatus where
    toEnum i = case i of
        0 -> BRSFailed
        1 -> BRSRunning
        2 -> BRSSucceeded
        _ -> (error . showText) $ "Unknown index for BusinessReportStatus: " <> toText i
    fromEnum x = case x of
        BRSFailed -> 0
        BRSRunning -> 1
        BRSSucceeded -> 2
        BusinessReportStatus' name -> (error . showText) $ "Unknown BusinessReportStatus: " <> original name

-- | Represents the bounds of /known/ $BusinessReportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BusinessReportStatus where
    minBound = BRSFailed
    maxBound = BRSSucceeded

instance Hashable     BusinessReportStatus
instance NFData       BusinessReportStatus
instance ToByteString BusinessReportStatus
instance ToQuery      BusinessReportStatus
instance ToHeader     BusinessReportStatus

instance FromJSON BusinessReportStatus where
    parseJSON = parseJSONText "BusinessReportStatus"
