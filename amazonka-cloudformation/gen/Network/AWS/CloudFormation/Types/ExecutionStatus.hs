{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ExecutionStatus (
  ExecutionStatus (
    ..
    , Available
    , ExecuteComplete
    , ExecuteFailed
    , ExecuteInProgress
    , Obsolete
    , Unavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExecutionStatus = ExecutionStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Available :: ExecutionStatus
pattern Available = ExecutionStatus' "AVAILABLE"

pattern ExecuteComplete :: ExecutionStatus
pattern ExecuteComplete = ExecutionStatus' "EXECUTE_COMPLETE"

pattern ExecuteFailed :: ExecutionStatus
pattern ExecuteFailed = ExecutionStatus' "EXECUTE_FAILED"

pattern ExecuteInProgress :: ExecutionStatus
pattern ExecuteInProgress = ExecutionStatus' "EXECUTE_IN_PROGRESS"

pattern Obsolete :: ExecutionStatus
pattern Obsolete = ExecutionStatus' "OBSOLETE"

pattern Unavailable :: ExecutionStatus
pattern Unavailable = ExecutionStatus' "UNAVAILABLE"

{-# COMPLETE
  Available,
  ExecuteComplete,
  ExecuteFailed,
  ExecuteInProgress,
  Obsolete,
  Unavailable,
  ExecutionStatus' #-}

instance FromText ExecutionStatus where
    parser = (ExecutionStatus' . mk) <$> takeText

instance ToText ExecutionStatus where
    toText (ExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExecutionStatus where
    toEnum i = case i of
        0 -> Available
        1 -> ExecuteComplete
        2 -> ExecuteFailed
        3 -> ExecuteInProgress
        4 -> Obsolete
        5 -> Unavailable
        _ -> (error . showText) $ "Unknown index for ExecutionStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        ExecuteComplete -> 1
        ExecuteFailed -> 2
        ExecuteInProgress -> 3
        Obsolete -> 4
        Unavailable -> 5
        ExecutionStatus' name -> (error . showText) $ "Unknown ExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExecutionStatus where
    minBound = Available
    maxBound = Unavailable

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromXML ExecutionStatus where
    parseXML = parseXMLText "ExecutionStatus"
