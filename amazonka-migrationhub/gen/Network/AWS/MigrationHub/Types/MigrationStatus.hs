{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.MigrationStatus (
  MigrationStatus (
    ..
    , MSCompleted
    , MSFailed
    , MSInProgress
    , MSNotStarted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MigrationStatus = MigrationStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MSCompleted :: MigrationStatus
pattern MSCompleted = MigrationStatus' "COMPLETED"

pattern MSFailed :: MigrationStatus
pattern MSFailed = MigrationStatus' "FAILED"

pattern MSInProgress :: MigrationStatus
pattern MSInProgress = MigrationStatus' "IN_PROGRESS"

pattern MSNotStarted :: MigrationStatus
pattern MSNotStarted = MigrationStatus' "NOT_STARTED"

{-# COMPLETE
  MSCompleted,
  MSFailed,
  MSInProgress,
  MSNotStarted,
  MigrationStatus' #-}

instance FromText MigrationStatus where
    parser = (MigrationStatus' . mk) <$> takeText

instance ToText MigrationStatus where
    toText (MigrationStatus' ci) = original ci

-- | Represents an enum of /known/ $MigrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MigrationStatus where
    toEnum i = case i of
        0 -> MSCompleted
        1 -> MSFailed
        2 -> MSInProgress
        3 -> MSNotStarted
        _ -> (error . showText) $ "Unknown index for MigrationStatus: " <> toText i
    fromEnum x = case x of
        MSCompleted -> 0
        MSFailed -> 1
        MSInProgress -> 2
        MSNotStarted -> 3
        MigrationStatus' name -> (error . showText) $ "Unknown MigrationStatus: " <> original name

-- | Represents the bounds of /known/ $MigrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MigrationStatus where
    minBound = MSCompleted
    maxBound = MSNotStarted

instance Hashable     MigrationStatus
instance NFData       MigrationStatus
instance ToByteString MigrationStatus
instance ToQuery      MigrationStatus
instance ToHeader     MigrationStatus

instance ToJSON MigrationStatus where
    toJSON = toJSONText

instance FromJSON MigrationStatus where
    parseJSON = parseJSONText "MigrationStatus"
