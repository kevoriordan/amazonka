{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppStatus (
  AppStatus (
    ..
    , ASDeleted
    , ASDeleting
    , ASFailed
    , ASInService
    , ASPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AppStatus = AppStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern ASDeleted :: AppStatus
pattern ASDeleted = AppStatus' "Deleted"

pattern ASDeleting :: AppStatus
pattern ASDeleting = AppStatus' "Deleting"

pattern ASFailed :: AppStatus
pattern ASFailed = AppStatus' "Failed"

pattern ASInService :: AppStatus
pattern ASInService = AppStatus' "InService"

pattern ASPending :: AppStatus
pattern ASPending = AppStatus' "Pending"

{-# COMPLETE
  ASDeleted,
  ASDeleting,
  ASFailed,
  ASInService,
  ASPending,
  AppStatus' #-}

instance FromText AppStatus where
    parser = (AppStatus' . mk) <$> takeText

instance ToText AppStatus where
    toText (AppStatus' ci) = original ci

-- | Represents an enum of /known/ $AppStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AppStatus where
    toEnum i = case i of
        0 -> ASDeleted
        1 -> ASDeleting
        2 -> ASFailed
        3 -> ASInService
        4 -> ASPending
        _ -> (error . showText) $ "Unknown index for AppStatus: " <> toText i
    fromEnum x = case x of
        ASDeleted -> 0
        ASDeleting -> 1
        ASFailed -> 2
        ASInService -> 3
        ASPending -> 4
        AppStatus' name -> (error . showText) $ "Unknown AppStatus: " <> original name

-- | Represents the bounds of /known/ $AppStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AppStatus where
    minBound = ASDeleted
    maxBound = ASPending

instance Hashable     AppStatus
instance NFData       AppStatus
instance ToByteString AppStatus
instance ToQuery      AppStatus
instance ToHeader     AppStatus

instance FromJSON AppStatus where
    parseJSON = parseJSONText "AppStatus"
