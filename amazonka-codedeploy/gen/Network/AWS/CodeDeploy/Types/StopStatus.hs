{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.StopStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.StopStatus (
  StopStatus (
    ..
    , SSPending
    , SSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StopStatus = StopStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern SSPending :: StopStatus
pattern SSPending = StopStatus' "Pending"

pattern SSSucceeded :: StopStatus
pattern SSSucceeded = StopStatus' "Succeeded"

{-# COMPLETE
  SSPending,
  SSSucceeded,
  StopStatus' #-}

instance FromText StopStatus where
    parser = (StopStatus' . mk) <$> takeText

instance ToText StopStatus where
    toText (StopStatus' ci) = original ci

-- | Represents an enum of /known/ $StopStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StopStatus where
    toEnum i = case i of
        0 -> SSPending
        1 -> SSSucceeded
        _ -> (error . showText) $ "Unknown index for StopStatus: " <> toText i
    fromEnum x = case x of
        SSPending -> 0
        SSSucceeded -> 1
        StopStatus' name -> (error . showText) $ "Unknown StopStatus: " <> original name

-- | Represents the bounds of /known/ $StopStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StopStatus where
    minBound = SSPending
    maxBound = SSSucceeded

instance Hashable     StopStatus
instance NFData       StopStatus
instance ToByteString StopStatus
instance ToQuery      StopStatus
instance ToHeader     StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"
