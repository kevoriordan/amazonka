{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.LDAPSStatus (
  LDAPSStatus (
    ..
    , LDAPSSDisabled
    , LDAPSSEnableFailed
    , LDAPSSEnabled
    , LDAPSSEnabling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LDAPSStatus = LDAPSStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern LDAPSSDisabled :: LDAPSStatus
pattern LDAPSSDisabled = LDAPSStatus' "Disabled"

pattern LDAPSSEnableFailed :: LDAPSStatus
pattern LDAPSSEnableFailed = LDAPSStatus' "EnableFailed"

pattern LDAPSSEnabled :: LDAPSStatus
pattern LDAPSSEnabled = LDAPSStatus' "Enabled"

pattern LDAPSSEnabling :: LDAPSStatus
pattern LDAPSSEnabling = LDAPSStatus' "Enabling"

{-# COMPLETE
  LDAPSSDisabled,
  LDAPSSEnableFailed,
  LDAPSSEnabled,
  LDAPSSEnabling,
  LDAPSStatus' #-}

instance FromText LDAPSStatus where
    parser = (LDAPSStatus' . mk) <$> takeText

instance ToText LDAPSStatus where
    toText (LDAPSStatus' ci) = original ci

-- | Represents an enum of /known/ $LDAPSStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LDAPSStatus where
    toEnum i = case i of
        0 -> LDAPSSDisabled
        1 -> LDAPSSEnableFailed
        2 -> LDAPSSEnabled
        3 -> LDAPSSEnabling
        _ -> (error . showText) $ "Unknown index for LDAPSStatus: " <> toText i
    fromEnum x = case x of
        LDAPSSDisabled -> 0
        LDAPSSEnableFailed -> 1
        LDAPSSEnabled -> 2
        LDAPSSEnabling -> 3
        LDAPSStatus' name -> (error . showText) $ "Unknown LDAPSStatus: " <> original name

-- | Represents the bounds of /known/ $LDAPSStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LDAPSStatus where
    minBound = LDAPSSDisabled
    maxBound = LDAPSSEnabling

instance Hashable     LDAPSStatus
instance NFData       LDAPSStatus
instance ToByteString LDAPSStatus
instance ToQuery      LDAPSStatus
instance ToHeader     LDAPSStatus

instance FromJSON LDAPSStatus where
    parseJSON = parseJSONText "LDAPSStatus"
