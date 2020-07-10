{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.HSMStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.HSMStatus (
  HSMStatus (
    ..
    , HSDegraded
    , HSPending
    , HSRunning
    , HSSuspended
    , HSTerminated
    , HSTerminating
    , HSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HSMStatus = HSMStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern HSDegraded :: HSMStatus
pattern HSDegraded = HSMStatus' "DEGRADED"

pattern HSPending :: HSMStatus
pattern HSPending = HSMStatus' "PENDING"

pattern HSRunning :: HSMStatus
pattern HSRunning = HSMStatus' "RUNNING"

pattern HSSuspended :: HSMStatus
pattern HSSuspended = HSMStatus' "SUSPENDED"

pattern HSTerminated :: HSMStatus
pattern HSTerminated = HSMStatus' "TERMINATED"

pattern HSTerminating :: HSMStatus
pattern HSTerminating = HSMStatus' "TERMINATING"

pattern HSUpdating :: HSMStatus
pattern HSUpdating = HSMStatus' "UPDATING"

{-# COMPLETE
  HSDegraded,
  HSPending,
  HSRunning,
  HSSuspended,
  HSTerminated,
  HSTerminating,
  HSUpdating,
  HSMStatus' #-}

instance FromText HSMStatus where
    parser = (HSMStatus' . mk) <$> takeText

instance ToText HSMStatus where
    toText (HSMStatus' ci) = original ci

-- | Represents an enum of /known/ $HSMStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HSMStatus where
    toEnum i = case i of
        0 -> HSDegraded
        1 -> HSPending
        2 -> HSRunning
        3 -> HSSuspended
        4 -> HSTerminated
        5 -> HSTerminating
        6 -> HSUpdating
        _ -> (error . showText) $ "Unknown index for HSMStatus: " <> toText i
    fromEnum x = case x of
        HSDegraded -> 0
        HSPending -> 1
        HSRunning -> 2
        HSSuspended -> 3
        HSTerminated -> 4
        HSTerminating -> 5
        HSUpdating -> 6
        HSMStatus' name -> (error . showText) $ "Unknown HSMStatus: " <> original name

-- | Represents the bounds of /known/ $HSMStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HSMStatus where
    minBound = HSDegraded
    maxBound = HSUpdating

instance Hashable     HSMStatus
instance NFData       HSMStatus
instance ToByteString HSMStatus
instance ToQuery      HSMStatus
instance ToHeader     HSMStatus

instance FromJSON HSMStatus where
    parseJSON = parseJSONText "HSMStatus"
