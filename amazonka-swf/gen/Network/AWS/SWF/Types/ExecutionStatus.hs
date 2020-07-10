{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ExecutionStatus (
  ExecutionStatus (
    ..
    , Closed
    , Open
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExecutionStatus = ExecutionStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Closed :: ExecutionStatus
pattern Closed = ExecutionStatus' "CLOSED"

pattern Open :: ExecutionStatus
pattern Open = ExecutionStatus' "OPEN"

{-# COMPLETE
  Closed,
  Open,
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
        0 -> Closed
        1 -> Open
        _ -> (error . showText) $ "Unknown index for ExecutionStatus: " <> toText i
    fromEnum x = case x of
        Closed -> 0
        Open -> 1
        ExecutionStatus' name -> (error . showText) $ "Unknown ExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExecutionStatus where
    minBound = Closed
    maxBound = Open

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"
