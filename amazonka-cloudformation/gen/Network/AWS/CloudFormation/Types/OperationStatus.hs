{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.OperationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.OperationStatus (
  OperationStatus (
    ..
    , OSFailed
    , OSInProgress
    , OSPending
    , OSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperationStatus = OperationStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern OSFailed :: OperationStatus
pattern OSFailed = OperationStatus' "FAILED"

pattern OSInProgress :: OperationStatus
pattern OSInProgress = OperationStatus' "IN_PROGRESS"

pattern OSPending :: OperationStatus
pattern OSPending = OperationStatus' "PENDING"

pattern OSSuccess :: OperationStatus
pattern OSSuccess = OperationStatus' "SUCCESS"

{-# COMPLETE
  OSFailed,
  OSInProgress,
  OSPending,
  OSSuccess,
  OperationStatus' #-}

instance FromText OperationStatus where
    parser = (OperationStatus' . mk) <$> takeText

instance ToText OperationStatus where
    toText (OperationStatus' ci) = original ci

-- | Represents an enum of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperationStatus where
    toEnum i = case i of
        0 -> OSFailed
        1 -> OSInProgress
        2 -> OSPending
        3 -> OSSuccess
        _ -> (error . showText) $ "Unknown index for OperationStatus: " <> toText i
    fromEnum x = case x of
        OSFailed -> 0
        OSInProgress -> 1
        OSPending -> 2
        OSSuccess -> 3
        OperationStatus' name -> (error . showText) $ "Unknown OperationStatus: " <> original name

-- | Represents the bounds of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperationStatus where
    minBound = OSFailed
    maxBound = OSSuccess

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus
