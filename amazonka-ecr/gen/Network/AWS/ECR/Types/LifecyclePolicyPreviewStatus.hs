{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus (
  LifecyclePolicyPreviewStatus (
    ..
    , LPPSComplete
    , LPPSExpired
    , LPPSFailed
    , LPPSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LifecyclePolicyPreviewStatus = LifecyclePolicyPreviewStatus' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern LPPSComplete :: LifecyclePolicyPreviewStatus
pattern LPPSComplete = LifecyclePolicyPreviewStatus' "COMPLETE"

pattern LPPSExpired :: LifecyclePolicyPreviewStatus
pattern LPPSExpired = LifecyclePolicyPreviewStatus' "EXPIRED"

pattern LPPSFailed :: LifecyclePolicyPreviewStatus
pattern LPPSFailed = LifecyclePolicyPreviewStatus' "FAILED"

pattern LPPSInProgress :: LifecyclePolicyPreviewStatus
pattern LPPSInProgress = LifecyclePolicyPreviewStatus' "IN_PROGRESS"

{-# COMPLETE
  LPPSComplete,
  LPPSExpired,
  LPPSFailed,
  LPPSInProgress,
  LifecyclePolicyPreviewStatus' #-}

instance FromText LifecyclePolicyPreviewStatus where
    parser = (LifecyclePolicyPreviewStatus' . mk) <$> takeText

instance ToText LifecyclePolicyPreviewStatus where
    toText (LifecyclePolicyPreviewStatus' ci) = original ci

-- | Represents an enum of /known/ $LifecyclePolicyPreviewStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LifecyclePolicyPreviewStatus where
    toEnum i = case i of
        0 -> LPPSComplete
        1 -> LPPSExpired
        2 -> LPPSFailed
        3 -> LPPSInProgress
        _ -> (error . showText) $ "Unknown index for LifecyclePolicyPreviewStatus: " <> toText i
    fromEnum x = case x of
        LPPSComplete -> 0
        LPPSExpired -> 1
        LPPSFailed -> 2
        LPPSInProgress -> 3
        LifecyclePolicyPreviewStatus' name -> (error . showText) $ "Unknown LifecyclePolicyPreviewStatus: " <> original name

-- | Represents the bounds of /known/ $LifecyclePolicyPreviewStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LifecyclePolicyPreviewStatus where
    minBound = LPPSComplete
    maxBound = LPPSInProgress

instance Hashable     LifecyclePolicyPreviewStatus
instance NFData       LifecyclePolicyPreviewStatus
instance ToByteString LifecyclePolicyPreviewStatus
instance ToQuery      LifecyclePolicyPreviewStatus
instance ToHeader     LifecyclePolicyPreviewStatus

instance FromJSON LifecyclePolicyPreviewStatus where
    parseJSON = parseJSONText "LifecyclePolicyPreviewStatus"
