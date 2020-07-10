{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.OTAUpdateStatus (
  OTAUpdateStatus (
    ..
    , CreateComplete
    , CreateFailed
    , CreateInProgress
    , CreatePending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OTAUpdateStatus = OTAUpdateStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CreateComplete :: OTAUpdateStatus
pattern CreateComplete = OTAUpdateStatus' "CREATE_COMPLETE"

pattern CreateFailed :: OTAUpdateStatus
pattern CreateFailed = OTAUpdateStatus' "CREATE_FAILED"

pattern CreateInProgress :: OTAUpdateStatus
pattern CreateInProgress = OTAUpdateStatus' "CREATE_IN_PROGRESS"

pattern CreatePending :: OTAUpdateStatus
pattern CreatePending = OTAUpdateStatus' "CREATE_PENDING"

{-# COMPLETE
  CreateComplete,
  CreateFailed,
  CreateInProgress,
  CreatePending,
  OTAUpdateStatus' #-}

instance FromText OTAUpdateStatus where
    parser = (OTAUpdateStatus' . mk) <$> takeText

instance ToText OTAUpdateStatus where
    toText (OTAUpdateStatus' ci) = original ci

-- | Represents an enum of /known/ $OTAUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OTAUpdateStatus where
    toEnum i = case i of
        0 -> CreateComplete
        1 -> CreateFailed
        2 -> CreateInProgress
        3 -> CreatePending
        _ -> (error . showText) $ "Unknown index for OTAUpdateStatus: " <> toText i
    fromEnum x = case x of
        CreateComplete -> 0
        CreateFailed -> 1
        CreateInProgress -> 2
        CreatePending -> 3
        OTAUpdateStatus' name -> (error . showText) $ "Unknown OTAUpdateStatus: " <> original name

-- | Represents the bounds of /known/ $OTAUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OTAUpdateStatus where
    minBound = CreateComplete
    maxBound = CreatePending

instance Hashable     OTAUpdateStatus
instance NFData       OTAUpdateStatus
instance ToByteString OTAUpdateStatus
instance ToQuery      OTAUpdateStatus
instance ToHeader     OTAUpdateStatus

instance ToJSON OTAUpdateStatus where
    toJSON = toJSONText

instance FromJSON OTAUpdateStatus where
    parseJSON = parseJSONText "OTAUpdateStatus"
