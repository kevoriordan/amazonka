{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BackupStatus (
  BackupStatus (
    ..
    , Available
    , Creating
    , Deleted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BackupStatus = BackupStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Available :: BackupStatus
pattern Available = BackupStatus' "AVAILABLE"

pattern Creating :: BackupStatus
pattern Creating = BackupStatus' "CREATING"

pattern Deleted :: BackupStatus
pattern Deleted = BackupStatus' "DELETED"

{-# COMPLETE
  Available,
  Creating,
  Deleted,
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
        0 -> Available
        1 -> Creating
        2 -> Deleted
        _ -> (error . showText) $ "Unknown index for BackupStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Creating -> 1
        Deleted -> 2
        BackupStatus' name -> (error . showText) $ "Unknown BackupStatus: " <> original name

-- | Represents the bounds of /known/ $BackupStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BackupStatus where
    minBound = Available
    maxBound = Deleted

instance Hashable     BackupStatus
instance NFData       BackupStatus
instance ToByteString BackupStatus
instance ToQuery      BackupStatus
instance ToHeader     BackupStatus

instance FromJSON BackupStatus where
    parseJSON = parseJSONText "BackupStatus"
