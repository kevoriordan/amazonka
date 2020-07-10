{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.MaintenanceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.MaintenanceStatus (
  MaintenanceStatus (
    ..
    , MSFailed
    , MSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MaintenanceStatus = MaintenanceStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern MSFailed :: MaintenanceStatus
pattern MSFailed = MaintenanceStatus' "FAILED"

pattern MSSuccess :: MaintenanceStatus
pattern MSSuccess = MaintenanceStatus' "SUCCESS"

{-# COMPLETE
  MSFailed,
  MSSuccess,
  MaintenanceStatus' #-}

instance FromText MaintenanceStatus where
    parser = (MaintenanceStatus' . mk) <$> takeText

instance ToText MaintenanceStatus where
    toText (MaintenanceStatus' ci) = original ci

-- | Represents an enum of /known/ $MaintenanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MaintenanceStatus where
    toEnum i = case i of
        0 -> MSFailed
        1 -> MSSuccess
        _ -> (error . showText) $ "Unknown index for MaintenanceStatus: " <> toText i
    fromEnum x = case x of
        MSFailed -> 0
        MSSuccess -> 1
        MaintenanceStatus' name -> (error . showText) $ "Unknown MaintenanceStatus: " <> original name

-- | Represents the bounds of /known/ $MaintenanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MaintenanceStatus where
    minBound = MSFailed
    maxBound = MSSuccess

instance Hashable     MaintenanceStatus
instance NFData       MaintenanceStatus
instance ToByteString MaintenanceStatus
instance ToQuery      MaintenanceStatus
instance ToHeader     MaintenanceStatus

instance FromJSON MaintenanceStatus where
    parseJSON = parseJSONText "MaintenanceStatus"
