{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ContributorInsightsStatus (
  ContributorInsightsStatus (
    ..
    , CISDisabled
    , CISDisabling
    , CISEnabled
    , CISEnabling
    , CISFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContributorInsightsStatus = ContributorInsightsStatus' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern CISDisabled :: ContributorInsightsStatus
pattern CISDisabled = ContributorInsightsStatus' "DISABLED"

pattern CISDisabling :: ContributorInsightsStatus
pattern CISDisabling = ContributorInsightsStatus' "DISABLING"

pattern CISEnabled :: ContributorInsightsStatus
pattern CISEnabled = ContributorInsightsStatus' "ENABLED"

pattern CISEnabling :: ContributorInsightsStatus
pattern CISEnabling = ContributorInsightsStatus' "ENABLING"

pattern CISFailed :: ContributorInsightsStatus
pattern CISFailed = ContributorInsightsStatus' "FAILED"

{-# COMPLETE
  CISDisabled,
  CISDisabling,
  CISEnabled,
  CISEnabling,
  CISFailed,
  ContributorInsightsStatus' #-}

instance FromText ContributorInsightsStatus where
    parser = (ContributorInsightsStatus' . mk) <$> takeText

instance ToText ContributorInsightsStatus where
    toText (ContributorInsightsStatus' ci) = original ci

-- | Represents an enum of /known/ $ContributorInsightsStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContributorInsightsStatus where
    toEnum i = case i of
        0 -> CISDisabled
        1 -> CISDisabling
        2 -> CISEnabled
        3 -> CISEnabling
        4 -> CISFailed
        _ -> (error . showText) $ "Unknown index for ContributorInsightsStatus: " <> toText i
    fromEnum x = case x of
        CISDisabled -> 0
        CISDisabling -> 1
        CISEnabled -> 2
        CISEnabling -> 3
        CISFailed -> 4
        ContributorInsightsStatus' name -> (error . showText) $ "Unknown ContributorInsightsStatus: " <> original name

-- | Represents the bounds of /known/ $ContributorInsightsStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContributorInsightsStatus where
    minBound = CISDisabled
    maxBound = CISFailed

instance Hashable     ContributorInsightsStatus
instance NFData       ContributorInsightsStatus
instance ToByteString ContributorInsightsStatus
instance ToQuery      ContributorInsightsStatus
instance ToHeader     ContributorInsightsStatus

instance FromJSON ContributorInsightsStatus where
    parseJSON = parseJSONText "ContributorInsightsStatus"
