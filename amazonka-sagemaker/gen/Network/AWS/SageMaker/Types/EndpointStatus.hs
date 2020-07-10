{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointStatus (
  EndpointStatus (
    ..
    , ESCreating
    , ESDeleting
    , ESFailed
    , ESInService
    , ESOutOfService
    , ESRollingBack
    , ESSystemUpdating
    , ESUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EndpointStatus = EndpointStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ESCreating :: EndpointStatus
pattern ESCreating = EndpointStatus' "Creating"

pattern ESDeleting :: EndpointStatus
pattern ESDeleting = EndpointStatus' "Deleting"

pattern ESFailed :: EndpointStatus
pattern ESFailed = EndpointStatus' "Failed"

pattern ESInService :: EndpointStatus
pattern ESInService = EndpointStatus' "InService"

pattern ESOutOfService :: EndpointStatus
pattern ESOutOfService = EndpointStatus' "OutOfService"

pattern ESRollingBack :: EndpointStatus
pattern ESRollingBack = EndpointStatus' "RollingBack"

pattern ESSystemUpdating :: EndpointStatus
pattern ESSystemUpdating = EndpointStatus' "SystemUpdating"

pattern ESUpdating :: EndpointStatus
pattern ESUpdating = EndpointStatus' "Updating"

{-# COMPLETE
  ESCreating,
  ESDeleting,
  ESFailed,
  ESInService,
  ESOutOfService,
  ESRollingBack,
  ESSystemUpdating,
  ESUpdating,
  EndpointStatus' #-}

instance FromText EndpointStatus where
    parser = (EndpointStatus' . mk) <$> takeText

instance ToText EndpointStatus where
    toText (EndpointStatus' ci) = original ci

-- | Represents an enum of /known/ $EndpointStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndpointStatus where
    toEnum i = case i of
        0 -> ESCreating
        1 -> ESDeleting
        2 -> ESFailed
        3 -> ESInService
        4 -> ESOutOfService
        5 -> ESRollingBack
        6 -> ESSystemUpdating
        7 -> ESUpdating
        _ -> (error . showText) $ "Unknown index for EndpointStatus: " <> toText i
    fromEnum x = case x of
        ESCreating -> 0
        ESDeleting -> 1
        ESFailed -> 2
        ESInService -> 3
        ESOutOfService -> 4
        ESRollingBack -> 5
        ESSystemUpdating -> 6
        ESUpdating -> 7
        EndpointStatus' name -> (error . showText) $ "Unknown EndpointStatus: " <> original name

-- | Represents the bounds of /known/ $EndpointStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndpointStatus where
    minBound = ESCreating
    maxBound = ESUpdating

instance Hashable     EndpointStatus
instance NFData       EndpointStatus
instance ToByteString EndpointStatus
instance ToQuery      EndpointStatus
instance ToHeader     EndpointStatus

instance ToJSON EndpointStatus where
    toJSON = toJSONText

instance FromJSON EndpointStatus where
    parseJSON = parseJSONText "EndpointStatus"
