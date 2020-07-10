{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus (
  ActionHistoryStatus (
    ..
    , AHSCompleted
    , AHSFailed
    , AHSUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionHistoryStatus = ActionHistoryStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern AHSCompleted :: ActionHistoryStatus
pattern AHSCompleted = ActionHistoryStatus' "Completed"

pattern AHSFailed :: ActionHistoryStatus
pattern AHSFailed = ActionHistoryStatus' "Failed"

pattern AHSUnknown :: ActionHistoryStatus
pattern AHSUnknown = ActionHistoryStatus' "Unknown"

{-# COMPLETE
  AHSCompleted,
  AHSFailed,
  AHSUnknown,
  ActionHistoryStatus' #-}

instance FromText ActionHistoryStatus where
    parser = (ActionHistoryStatus' . mk) <$> takeText

instance ToText ActionHistoryStatus where
    toText (ActionHistoryStatus' ci) = original ci

-- | Represents an enum of /known/ $ActionHistoryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionHistoryStatus where
    toEnum i = case i of
        0 -> AHSCompleted
        1 -> AHSFailed
        2 -> AHSUnknown
        _ -> (error . showText) $ "Unknown index for ActionHistoryStatus: " <> toText i
    fromEnum x = case x of
        AHSCompleted -> 0
        AHSFailed -> 1
        AHSUnknown -> 2
        ActionHistoryStatus' name -> (error . showText) $ "Unknown ActionHistoryStatus: " <> original name

-- | Represents the bounds of /known/ $ActionHistoryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionHistoryStatus where
    minBound = AHSCompleted
    maxBound = AHSUnknown

instance Hashable     ActionHistoryStatus
instance NFData       ActionHistoryStatus
instance ToByteString ActionHistoryStatus
instance ToQuery      ActionHistoryStatus
instance ToHeader     ActionHistoryStatus

instance FromXML ActionHistoryStatus where
    parseXML = parseXMLText "ActionHistoryStatus"
