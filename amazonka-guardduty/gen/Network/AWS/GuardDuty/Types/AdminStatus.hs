{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AdminStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AdminStatus (
  AdminStatus (
    ..
    , DisableInProgress
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AdminStatus = AdminStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern DisableInProgress :: AdminStatus
pattern DisableInProgress = AdminStatus' "DISABLE_IN_PROGRESS"

pattern Enabled :: AdminStatus
pattern Enabled = AdminStatus' "ENABLED"

{-# COMPLETE
  DisableInProgress,
  Enabled,
  AdminStatus' #-}

instance FromText AdminStatus where
    parser = (AdminStatus' . mk) <$> takeText

instance ToText AdminStatus where
    toText (AdminStatus' ci) = original ci

-- | Represents an enum of /known/ $AdminStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdminStatus where
    toEnum i = case i of
        0 -> DisableInProgress
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for AdminStatus: " <> toText i
    fromEnum x = case x of
        DisableInProgress -> 0
        Enabled -> 1
        AdminStatus' name -> (error . showText) $ "Unknown AdminStatus: " <> original name

-- | Represents the bounds of /known/ $AdminStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdminStatus where
    minBound = DisableInProgress
    maxBound = Enabled

instance Hashable     AdminStatus
instance NFData       AdminStatus
instance ToByteString AdminStatus
instance ToQuery      AdminStatus
instance ToHeader     AdminStatus

instance FromJSON AdminStatus where
    parseJSON = parseJSONText "AdminStatus"
