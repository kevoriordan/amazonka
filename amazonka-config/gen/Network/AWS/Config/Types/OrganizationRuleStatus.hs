{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationRuleStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationRuleStatus (
  OrganizationRuleStatus (
    ..
    , OCreateFailed
    , OCreateInProgress
    , OCreateSuccessful
    , ODeleteFailed
    , ODeleteInProgress
    , ODeleteSuccessful
    , OUpdateFailed
    , OUpdateInProgress
    , OUpdateSuccessful
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrganizationRuleStatus = OrganizationRuleStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern OCreateFailed :: OrganizationRuleStatus
pattern OCreateFailed = OrganizationRuleStatus' "CREATE_FAILED"

pattern OCreateInProgress :: OrganizationRuleStatus
pattern OCreateInProgress = OrganizationRuleStatus' "CREATE_IN_PROGRESS"

pattern OCreateSuccessful :: OrganizationRuleStatus
pattern OCreateSuccessful = OrganizationRuleStatus' "CREATE_SUCCESSFUL"

pattern ODeleteFailed :: OrganizationRuleStatus
pattern ODeleteFailed = OrganizationRuleStatus' "DELETE_FAILED"

pattern ODeleteInProgress :: OrganizationRuleStatus
pattern ODeleteInProgress = OrganizationRuleStatus' "DELETE_IN_PROGRESS"

pattern ODeleteSuccessful :: OrganizationRuleStatus
pattern ODeleteSuccessful = OrganizationRuleStatus' "DELETE_SUCCESSFUL"

pattern OUpdateFailed :: OrganizationRuleStatus
pattern OUpdateFailed = OrganizationRuleStatus' "UPDATE_FAILED"

pattern OUpdateInProgress :: OrganizationRuleStatus
pattern OUpdateInProgress = OrganizationRuleStatus' "UPDATE_IN_PROGRESS"

pattern OUpdateSuccessful :: OrganizationRuleStatus
pattern OUpdateSuccessful = OrganizationRuleStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  OCreateFailed,
  OCreateInProgress,
  OCreateSuccessful,
  ODeleteFailed,
  ODeleteInProgress,
  ODeleteSuccessful,
  OUpdateFailed,
  OUpdateInProgress,
  OUpdateSuccessful,
  OrganizationRuleStatus' #-}

instance FromText OrganizationRuleStatus where
    parser = (OrganizationRuleStatus' . mk) <$> takeText

instance ToText OrganizationRuleStatus where
    toText (OrganizationRuleStatus' ci) = original ci

-- | Represents an enum of /known/ $OrganizationRuleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrganizationRuleStatus where
    toEnum i = case i of
        0 -> OCreateFailed
        1 -> OCreateInProgress
        2 -> OCreateSuccessful
        3 -> ODeleteFailed
        4 -> ODeleteInProgress
        5 -> ODeleteSuccessful
        6 -> OUpdateFailed
        7 -> OUpdateInProgress
        8 -> OUpdateSuccessful
        _ -> (error . showText) $ "Unknown index for OrganizationRuleStatus: " <> toText i
    fromEnum x = case x of
        OCreateFailed -> 0
        OCreateInProgress -> 1
        OCreateSuccessful -> 2
        ODeleteFailed -> 3
        ODeleteInProgress -> 4
        ODeleteSuccessful -> 5
        OUpdateFailed -> 6
        OUpdateInProgress -> 7
        OUpdateSuccessful -> 8
        OrganizationRuleStatus' name -> (error . showText) $ "Unknown OrganizationRuleStatus: " <> original name

-- | Represents the bounds of /known/ $OrganizationRuleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrganizationRuleStatus where
    minBound = OCreateFailed
    maxBound = OUpdateSuccessful

instance Hashable     OrganizationRuleStatus
instance NFData       OrganizationRuleStatus
instance ToByteString OrganizationRuleStatus
instance ToQuery      OrganizationRuleStatus
instance ToHeader     OrganizationRuleStatus

instance FromJSON OrganizationRuleStatus where
    parseJSON = parseJSONText "OrganizationRuleStatus"
