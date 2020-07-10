{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTaskState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.BundleTaskState (
  BundleTaskState (
    ..
    , BTSBundling
    , BTSCancelling
    , BTSComplete
    , BTSFailed
    , BTSPending
    , BTSStoring
    , BTSWaitingForShutdown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data BundleTaskState = BundleTaskState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern BTSBundling :: BundleTaskState
pattern BTSBundling = BundleTaskState' "bundling"

pattern BTSCancelling :: BundleTaskState
pattern BTSCancelling = BundleTaskState' "cancelling"

pattern BTSComplete :: BundleTaskState
pattern BTSComplete = BundleTaskState' "complete"

pattern BTSFailed :: BundleTaskState
pattern BTSFailed = BundleTaskState' "failed"

pattern BTSPending :: BundleTaskState
pattern BTSPending = BundleTaskState' "pending"

pattern BTSStoring :: BundleTaskState
pattern BTSStoring = BundleTaskState' "storing"

pattern BTSWaitingForShutdown :: BundleTaskState
pattern BTSWaitingForShutdown = BundleTaskState' "waiting-for-shutdown"

{-# COMPLETE
  BTSBundling,
  BTSCancelling,
  BTSComplete,
  BTSFailed,
  BTSPending,
  BTSStoring,
  BTSWaitingForShutdown,
  BundleTaskState' #-}

instance FromText BundleTaskState where
    parser = (BundleTaskState' . mk) <$> takeText

instance ToText BundleTaskState where
    toText (BundleTaskState' ci) = original ci

-- | Represents an enum of /known/ $BundleTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BundleTaskState where
    toEnum i = case i of
        0 -> BTSBundling
        1 -> BTSCancelling
        2 -> BTSComplete
        3 -> BTSFailed
        4 -> BTSPending
        5 -> BTSStoring
        6 -> BTSWaitingForShutdown
        _ -> (error . showText) $ "Unknown index for BundleTaskState: " <> toText i
    fromEnum x = case x of
        BTSBundling -> 0
        BTSCancelling -> 1
        BTSComplete -> 2
        BTSFailed -> 3
        BTSPending -> 4
        BTSStoring -> 5
        BTSWaitingForShutdown -> 6
        BundleTaskState' name -> (error . showText) $ "Unknown BundleTaskState: " <> original name

-- | Represents the bounds of /known/ $BundleTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BundleTaskState where
    minBound = BTSBundling
    maxBound = BTSWaitingForShutdown

instance Hashable     BundleTaskState
instance NFData       BundleTaskState
instance ToByteString BundleTaskState
instance ToQuery      BundleTaskState
instance ToHeader     BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"
