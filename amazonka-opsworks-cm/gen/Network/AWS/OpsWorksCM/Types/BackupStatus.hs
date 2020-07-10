{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.BackupStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.BackupStatus (
  BackupStatus (
    ..
    , BSDeleting
    , BSFailed
    , BSInProgress
    , BSOK
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BackupStatus = BackupStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern BSDeleting :: BackupStatus
pattern BSDeleting = BackupStatus' "DELETING"

pattern BSFailed :: BackupStatus
pattern BSFailed = BackupStatus' "FAILED"

pattern BSInProgress :: BackupStatus
pattern BSInProgress = BackupStatus' "IN_PROGRESS"

pattern BSOK :: BackupStatus
pattern BSOK = BackupStatus' "OK"

{-# COMPLETE
  BSDeleting,
  BSFailed,
  BSInProgress,
  BSOK,
  BackupStatus' #-}

instance FromText BackupStatus where
    parser = (BackupStatus' . mk) <$> takeText

instance ToText BackupStatus where
    toText (BackupStatus' ci) = original ci

-- | Represents an enum of /known/ $BackupStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BackupStatus where
    toEnum i = case i of
        0 -> BSDeleting
        1 -> BSFailed
        2 -> BSInProgress
        3 -> BSOK
        _ -> (error . showText) $ "Unknown index for BackupStatus: " <> toText i
    fromEnum x = case x of
        BSDeleting -> 0
        BSFailed -> 1
        BSInProgress -> 2
        BSOK -> 3
        BackupStatus' name -> (error . showText) $ "Unknown BackupStatus: " <> original name

-- | Represents the bounds of /known/ $BackupStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BackupStatus where
    minBound = BSDeleting
    maxBound = BSOK

instance Hashable     BackupStatus
instance NFData       BackupStatus
instance ToByteString BackupStatus
instance ToQuery      BackupStatus
instance ToHeader     BackupStatus

instance FromJSON BackupStatus where
    parseJSON = parseJSONText "BackupStatus"
