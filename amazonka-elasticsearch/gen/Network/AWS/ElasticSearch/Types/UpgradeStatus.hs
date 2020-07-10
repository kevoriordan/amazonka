{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.UpgradeStatus (
  UpgradeStatus (
    ..
    , USFailed
    , USInProgress
    , USSucceeded
    , USSucceededWithIssues
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UpgradeStatus = UpgradeStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern USFailed :: UpgradeStatus
pattern USFailed = UpgradeStatus' "FAILED"

pattern USInProgress :: UpgradeStatus
pattern USInProgress = UpgradeStatus' "IN_PROGRESS"

pattern USSucceeded :: UpgradeStatus
pattern USSucceeded = UpgradeStatus' "SUCCEEDED"

pattern USSucceededWithIssues :: UpgradeStatus
pattern USSucceededWithIssues = UpgradeStatus' "SUCCEEDED_WITH_ISSUES"

{-# COMPLETE
  USFailed,
  USInProgress,
  USSucceeded,
  USSucceededWithIssues,
  UpgradeStatus' #-}

instance FromText UpgradeStatus where
    parser = (UpgradeStatus' . mk) <$> takeText

instance ToText UpgradeStatus where
    toText (UpgradeStatus' ci) = original ci

-- | Represents an enum of /known/ $UpgradeStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UpgradeStatus where
    toEnum i = case i of
        0 -> USFailed
        1 -> USInProgress
        2 -> USSucceeded
        3 -> USSucceededWithIssues
        _ -> (error . showText) $ "Unknown index for UpgradeStatus: " <> toText i
    fromEnum x = case x of
        USFailed -> 0
        USInProgress -> 1
        USSucceeded -> 2
        USSucceededWithIssues -> 3
        UpgradeStatus' name -> (error . showText) $ "Unknown UpgradeStatus: " <> original name

-- | Represents the bounds of /known/ $UpgradeStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UpgradeStatus where
    minBound = USFailed
    maxBound = USSucceededWithIssues

instance Hashable     UpgradeStatus
instance NFData       UpgradeStatus
instance ToByteString UpgradeStatus
instance ToQuery      UpgradeStatus
instance ToHeader     UpgradeStatus

instance FromJSON UpgradeStatus where
    parseJSON = parseJSONText "UpgradeStatus"
