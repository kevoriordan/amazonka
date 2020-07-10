{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignStatus (
  CampaignStatus (
    ..
    , Completed
    , Deleted
    , Executing
    , Paused
    , PendingNextRun
    , Scheduled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CampaignStatus = CampaignStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Completed :: CampaignStatus
pattern Completed = CampaignStatus' "COMPLETED"

pattern Deleted :: CampaignStatus
pattern Deleted = CampaignStatus' "DELETED"

pattern Executing :: CampaignStatus
pattern Executing = CampaignStatus' "EXECUTING"

pattern Paused :: CampaignStatus
pattern Paused = CampaignStatus' "PAUSED"

pattern PendingNextRun :: CampaignStatus
pattern PendingNextRun = CampaignStatus' "PENDING_NEXT_RUN"

pattern Scheduled :: CampaignStatus
pattern Scheduled = CampaignStatus' "SCHEDULED"

{-# COMPLETE
  Completed,
  Deleted,
  Executing,
  Paused,
  PendingNextRun,
  Scheduled,
  CampaignStatus' #-}

instance FromText CampaignStatus where
    parser = (CampaignStatus' . mk) <$> takeText

instance ToText CampaignStatus where
    toText (CampaignStatus' ci) = original ci

-- | Represents an enum of /known/ $CampaignStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CampaignStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> Deleted
        2 -> Executing
        3 -> Paused
        4 -> PendingNextRun
        5 -> Scheduled
        _ -> (error . showText) $ "Unknown index for CampaignStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Deleted -> 1
        Executing -> 2
        Paused -> 3
        PendingNextRun -> 4
        Scheduled -> 5
        CampaignStatus' name -> (error . showText) $ "Unknown CampaignStatus: " <> original name

-- | Represents the bounds of /known/ $CampaignStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CampaignStatus where
    minBound = Completed
    maxBound = Scheduled

instance Hashable     CampaignStatus
instance NFData       CampaignStatus
instance ToByteString CampaignStatus
instance ToQuery      CampaignStatus
instance ToHeader     CampaignStatus

instance FromJSON CampaignStatus where
    parseJSON = parseJSONText "CampaignStatus"
