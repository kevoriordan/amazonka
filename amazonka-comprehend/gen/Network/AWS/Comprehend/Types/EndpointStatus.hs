{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EndpointStatus (
  EndpointStatus (
    ..
    , ESCreating
    , ESDeleting
    , ESFailed
    , ESInService
    , ESUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EndpointStatus = EndpointStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ESCreating :: EndpointStatus
pattern ESCreating = EndpointStatus' "CREATING"

pattern ESDeleting :: EndpointStatus
pattern ESDeleting = EndpointStatus' "DELETING"

pattern ESFailed :: EndpointStatus
pattern ESFailed = EndpointStatus' "FAILED"

pattern ESInService :: EndpointStatus
pattern ESInService = EndpointStatus' "IN_SERVICE"

pattern ESUpdating :: EndpointStatus
pattern ESUpdating = EndpointStatus' "UPDATING"

{-# COMPLETE
  ESCreating,
  ESDeleting,
  ESFailed,
  ESInService,
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
        4 -> ESUpdating
        _ -> (error . showText) $ "Unknown index for EndpointStatus: " <> toText i
    fromEnum x = case x of
        ESCreating -> 0
        ESDeleting -> 1
        ESFailed -> 2
        ESInService -> 3
        ESUpdating -> 4
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
