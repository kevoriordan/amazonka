{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.BulkPublishStatus (
  BulkPublishStatus (
    ..
    , Failed
    , InProgress
    , NotStarted
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BulkPublishStatus = BulkPublishStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Failed :: BulkPublishStatus
pattern Failed = BulkPublishStatus' "FAILED"

pattern InProgress :: BulkPublishStatus
pattern InProgress = BulkPublishStatus' "IN_PROGRESS"

pattern NotStarted :: BulkPublishStatus
pattern NotStarted = BulkPublishStatus' "NOT_STARTED"

pattern Succeeded :: BulkPublishStatus
pattern Succeeded = BulkPublishStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  NotStarted,
  Succeeded,
  BulkPublishStatus' #-}

instance FromText BulkPublishStatus where
    parser = (BulkPublishStatus' . mk) <$> takeText

instance ToText BulkPublishStatus where
    toText (BulkPublishStatus' ci) = original ci

-- | Represents an enum of /known/ $BulkPublishStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BulkPublishStatus where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> NotStarted
        3 -> Succeeded
        _ -> (error . showText) $ "Unknown index for BulkPublishStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        NotStarted -> 2
        Succeeded -> 3
        BulkPublishStatus' name -> (error . showText) $ "Unknown BulkPublishStatus: " <> original name

-- | Represents the bounds of /known/ $BulkPublishStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BulkPublishStatus where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     BulkPublishStatus
instance NFData       BulkPublishStatus
instance ToByteString BulkPublishStatus
instance ToQuery      BulkPublishStatus
instance ToHeader     BulkPublishStatus

instance FromJSON BulkPublishStatus where
    parseJSON = parseJSONText "BulkPublishStatus"
