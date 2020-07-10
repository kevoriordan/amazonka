{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus (
  TrialComponentPrimaryStatus (
    ..
    , TCPSCompleted
    , TCPSFailed
    , TCPSInProgress
    , TCPSStopped
    , TCPSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrialComponentPrimaryStatus = TrialComponentPrimaryStatus' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern TCPSCompleted :: TrialComponentPrimaryStatus
pattern TCPSCompleted = TrialComponentPrimaryStatus' "Completed"

pattern TCPSFailed :: TrialComponentPrimaryStatus
pattern TCPSFailed = TrialComponentPrimaryStatus' "Failed"

pattern TCPSInProgress :: TrialComponentPrimaryStatus
pattern TCPSInProgress = TrialComponentPrimaryStatus' "InProgress"

pattern TCPSStopped :: TrialComponentPrimaryStatus
pattern TCPSStopped = TrialComponentPrimaryStatus' "Stopped"

pattern TCPSStopping :: TrialComponentPrimaryStatus
pattern TCPSStopping = TrialComponentPrimaryStatus' "Stopping"

{-# COMPLETE
  TCPSCompleted,
  TCPSFailed,
  TCPSInProgress,
  TCPSStopped,
  TCPSStopping,
  TrialComponentPrimaryStatus' #-}

instance FromText TrialComponentPrimaryStatus where
    parser = (TrialComponentPrimaryStatus' . mk) <$> takeText

instance ToText TrialComponentPrimaryStatus where
    toText (TrialComponentPrimaryStatus' ci) = original ci

-- | Represents an enum of /known/ $TrialComponentPrimaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrialComponentPrimaryStatus where
    toEnum i = case i of
        0 -> TCPSCompleted
        1 -> TCPSFailed
        2 -> TCPSInProgress
        3 -> TCPSStopped
        4 -> TCPSStopping
        _ -> (error . showText) $ "Unknown index for TrialComponentPrimaryStatus: " <> toText i
    fromEnum x = case x of
        TCPSCompleted -> 0
        TCPSFailed -> 1
        TCPSInProgress -> 2
        TCPSStopped -> 3
        TCPSStopping -> 4
        TrialComponentPrimaryStatus' name -> (error . showText) $ "Unknown TrialComponentPrimaryStatus: " <> original name

-- | Represents the bounds of /known/ $TrialComponentPrimaryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrialComponentPrimaryStatus where
    minBound = TCPSCompleted
    maxBound = TCPSStopping

instance Hashable     TrialComponentPrimaryStatus
instance NFData       TrialComponentPrimaryStatus
instance ToByteString TrialComponentPrimaryStatus
instance ToQuery      TrialComponentPrimaryStatus
instance ToHeader     TrialComponentPrimaryStatus

instance ToJSON TrialComponentPrimaryStatus where
    toJSON = toJSONText

instance FromJSON TrialComponentPrimaryStatus where
    parseJSON = parseJSONText "TrialComponentPrimaryStatus"
