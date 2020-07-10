{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationResourceDetailedStatus (
  OrganizationResourceDetailedStatus (
    ..
    , CreateFailed
    , CreateInProgress
    , CreateSuccessful
    , DeleteFailed
    , DeleteInProgress
    , DeleteSuccessful
    , UpdateFailed
    , UpdateInProgress
    , UpdateSuccessful
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrganizationResourceDetailedStatus = OrganizationResourceDetailedStatus' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern CreateFailed :: OrganizationResourceDetailedStatus
pattern CreateFailed = OrganizationResourceDetailedStatus' "CREATE_FAILED"

pattern CreateInProgress :: OrganizationResourceDetailedStatus
pattern CreateInProgress = OrganizationResourceDetailedStatus' "CREATE_IN_PROGRESS"

pattern CreateSuccessful :: OrganizationResourceDetailedStatus
pattern CreateSuccessful = OrganizationResourceDetailedStatus' "CREATE_SUCCESSFUL"

pattern DeleteFailed :: OrganizationResourceDetailedStatus
pattern DeleteFailed = OrganizationResourceDetailedStatus' "DELETE_FAILED"

pattern DeleteInProgress :: OrganizationResourceDetailedStatus
pattern DeleteInProgress = OrganizationResourceDetailedStatus' "DELETE_IN_PROGRESS"

pattern DeleteSuccessful :: OrganizationResourceDetailedStatus
pattern DeleteSuccessful = OrganizationResourceDetailedStatus' "DELETE_SUCCESSFUL"

pattern UpdateFailed :: OrganizationResourceDetailedStatus
pattern UpdateFailed = OrganizationResourceDetailedStatus' "UPDATE_FAILED"

pattern UpdateInProgress :: OrganizationResourceDetailedStatus
pattern UpdateInProgress = OrganizationResourceDetailedStatus' "UPDATE_IN_PROGRESS"

pattern UpdateSuccessful :: OrganizationResourceDetailedStatus
pattern UpdateSuccessful = OrganizationResourceDetailedStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  CreateFailed,
  CreateInProgress,
  CreateSuccessful,
  DeleteFailed,
  DeleteInProgress,
  DeleteSuccessful,
  UpdateFailed,
  UpdateInProgress,
  UpdateSuccessful,
  OrganizationResourceDetailedStatus' #-}

instance FromText OrganizationResourceDetailedStatus where
    parser = (OrganizationResourceDetailedStatus' . mk) <$> takeText

instance ToText OrganizationResourceDetailedStatus where
    toText (OrganizationResourceDetailedStatus' ci) = original ci

-- | Represents an enum of /known/ $OrganizationResourceDetailedStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrganizationResourceDetailedStatus where
    toEnum i = case i of
        0 -> CreateFailed
        1 -> CreateInProgress
        2 -> CreateSuccessful
        3 -> DeleteFailed
        4 -> DeleteInProgress
        5 -> DeleteSuccessful
        6 -> UpdateFailed
        7 -> UpdateInProgress
        8 -> UpdateSuccessful
        _ -> (error . showText) $ "Unknown index for OrganizationResourceDetailedStatus: " <> toText i
    fromEnum x = case x of
        CreateFailed -> 0
        CreateInProgress -> 1
        CreateSuccessful -> 2
        DeleteFailed -> 3
        DeleteInProgress -> 4
        DeleteSuccessful -> 5
        UpdateFailed -> 6
        UpdateInProgress -> 7
        UpdateSuccessful -> 8
        OrganizationResourceDetailedStatus' name -> (error . showText) $ "Unknown OrganizationResourceDetailedStatus: " <> original name

-- | Represents the bounds of /known/ $OrganizationResourceDetailedStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrganizationResourceDetailedStatus where
    minBound = CreateFailed
    maxBound = UpdateSuccessful

instance Hashable     OrganizationResourceDetailedStatus
instance NFData       OrganizationResourceDetailedStatus
instance ToByteString OrganizationResourceDetailedStatus
instance ToQuery      OrganizationResourceDetailedStatus
instance ToHeader     OrganizationResourceDetailedStatus

instance ToJSON OrganizationResourceDetailedStatus where
    toJSON = toJSONText

instance FromJSON OrganizationResourceDetailedStatus where
    parseJSON = parseJSONText "OrganizationResourceDetailedStatus"
