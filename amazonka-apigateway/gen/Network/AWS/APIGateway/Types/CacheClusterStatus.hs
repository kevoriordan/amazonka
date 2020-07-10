{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.CacheClusterStatus (
  CacheClusterStatus (
    ..
    , Available
    , CreateInProgress
    , DeleteInProgress
    , FlushInProgress
    , NotAvailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Returns the status of the __CacheCluster__ .
--
--
data CacheClusterStatus = CacheClusterStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Available :: CacheClusterStatus
pattern Available = CacheClusterStatus' "AVAILABLE"

pattern CreateInProgress :: CacheClusterStatus
pattern CreateInProgress = CacheClusterStatus' "CREATE_IN_PROGRESS"

pattern DeleteInProgress :: CacheClusterStatus
pattern DeleteInProgress = CacheClusterStatus' "DELETE_IN_PROGRESS"

pattern FlushInProgress :: CacheClusterStatus
pattern FlushInProgress = CacheClusterStatus' "FLUSH_IN_PROGRESS"

pattern NotAvailable :: CacheClusterStatus
pattern NotAvailable = CacheClusterStatus' "NOT_AVAILABLE"

{-# COMPLETE
  Available,
  CreateInProgress,
  DeleteInProgress,
  FlushInProgress,
  NotAvailable,
  CacheClusterStatus' #-}

instance FromText CacheClusterStatus where
    parser = (CacheClusterStatus' . mk) <$> takeText

instance ToText CacheClusterStatus where
    toText (CacheClusterStatus' ci) = original ci

-- | Represents an enum of /known/ $CacheClusterStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CacheClusterStatus where
    toEnum i = case i of
        0 -> Available
        1 -> CreateInProgress
        2 -> DeleteInProgress
        3 -> FlushInProgress
        4 -> NotAvailable
        _ -> (error . showText) $ "Unknown index for CacheClusterStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        CreateInProgress -> 1
        DeleteInProgress -> 2
        FlushInProgress -> 3
        NotAvailable -> 4
        CacheClusterStatus' name -> (error . showText) $ "Unknown CacheClusterStatus: " <> original name

-- | Represents the bounds of /known/ $CacheClusterStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CacheClusterStatus where
    minBound = Available
    maxBound = NotAvailable

instance Hashable     CacheClusterStatus
instance NFData       CacheClusterStatus
instance ToByteString CacheClusterStatus
instance ToQuery      CacheClusterStatus
instance ToHeader     CacheClusterStatus

instance FromJSON CacheClusterStatus where
    parseJSON = parseJSONText "CacheClusterStatus"
