{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatastoreStatus (
  DatastoreStatus (
    ..
    , DSActive
    , DSCreating
    , DSDeleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DatastoreStatus = DatastoreStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern DSActive :: DatastoreStatus
pattern DSActive = DatastoreStatus' "ACTIVE"

pattern DSCreating :: DatastoreStatus
pattern DSCreating = DatastoreStatus' "CREATING"

pattern DSDeleting :: DatastoreStatus
pattern DSDeleting = DatastoreStatus' "DELETING"

{-# COMPLETE
  DSActive,
  DSCreating,
  DSDeleting,
  DatastoreStatus' #-}

instance FromText DatastoreStatus where
    parser = (DatastoreStatus' . mk) <$> takeText

instance ToText DatastoreStatus where
    toText (DatastoreStatus' ci) = original ci

-- | Represents an enum of /known/ $DatastoreStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DatastoreStatus where
    toEnum i = case i of
        0 -> DSActive
        1 -> DSCreating
        2 -> DSDeleting
        _ -> (error . showText) $ "Unknown index for DatastoreStatus: " <> toText i
    fromEnum x = case x of
        DSActive -> 0
        DSCreating -> 1
        DSDeleting -> 2
        DatastoreStatus' name -> (error . showText) $ "Unknown DatastoreStatus: " <> original name

-- | Represents the bounds of /known/ $DatastoreStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DatastoreStatus where
    minBound = DSActive
    maxBound = DSDeleting

instance Hashable     DatastoreStatus
instance NFData       DatastoreStatus
instance ToByteString DatastoreStatus
instance ToQuery      DatastoreStatus
instance ToHeader     DatastoreStatus

instance FromJSON DatastoreStatus where
    parseJSON = parseJSONText "DatastoreStatus"
