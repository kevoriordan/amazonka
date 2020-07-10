{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ServiceUpdateStatus (
  ServiceUpdateStatus (
    ..
    , Available
    , Cancelled
    , Expired
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceUpdateStatus = ServiceUpdateStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Available :: ServiceUpdateStatus
pattern Available = ServiceUpdateStatus' "available"

pattern Cancelled :: ServiceUpdateStatus
pattern Cancelled = ServiceUpdateStatus' "cancelled"

pattern Expired :: ServiceUpdateStatus
pattern Expired = ServiceUpdateStatus' "expired"

{-# COMPLETE
  Available,
  Cancelled,
  Expired,
  ServiceUpdateStatus' #-}

instance FromText ServiceUpdateStatus where
    parser = (ServiceUpdateStatus' . mk) <$> takeText

instance ToText ServiceUpdateStatus where
    toText (ServiceUpdateStatus' ci) = original ci

-- | Represents an enum of /known/ $ServiceUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceUpdateStatus where
    toEnum i = case i of
        0 -> Available
        1 -> Cancelled
        2 -> Expired
        _ -> (error . showText) $ "Unknown index for ServiceUpdateStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Cancelled -> 1
        Expired -> 2
        ServiceUpdateStatus' name -> (error . showText) $ "Unknown ServiceUpdateStatus: " <> original name

-- | Represents the bounds of /known/ $ServiceUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceUpdateStatus where
    minBound = Available
    maxBound = Expired

instance Hashable     ServiceUpdateStatus
instance NFData       ServiceUpdateStatus
instance ToByteString ServiceUpdateStatus
instance ToQuery      ServiceUpdateStatus
instance ToHeader     ServiceUpdateStatus

instance FromXML ServiceUpdateStatus where
    parseXML = parseXMLText "ServiceUpdateStatus"
