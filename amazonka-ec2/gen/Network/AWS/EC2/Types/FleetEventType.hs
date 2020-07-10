{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetEventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetEventType (
  FleetEventType (
    ..
    , FleetEventFleetChange
    , FleetEventInstanceChange
    , FleetEventServiceError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetEventType = FleetEventType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern FleetEventFleetChange :: FleetEventType
pattern FleetEventFleetChange = FleetEventType' "fleet-change"

pattern FleetEventInstanceChange :: FleetEventType
pattern FleetEventInstanceChange = FleetEventType' "instance-change"

pattern FleetEventServiceError :: FleetEventType
pattern FleetEventServiceError = FleetEventType' "service-error"

{-# COMPLETE
  FleetEventFleetChange,
  FleetEventInstanceChange,
  FleetEventServiceError,
  FleetEventType' #-}

instance FromText FleetEventType where
    parser = (FleetEventType' . mk) <$> takeText

instance ToText FleetEventType where
    toText (FleetEventType' ci) = original ci

-- | Represents an enum of /known/ $FleetEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetEventType where
    toEnum i = case i of
        0 -> FleetEventFleetChange
        1 -> FleetEventInstanceChange
        2 -> FleetEventServiceError
        _ -> (error . showText) $ "Unknown index for FleetEventType: " <> toText i
    fromEnum x = case x of
        FleetEventFleetChange -> 0
        FleetEventInstanceChange -> 1
        FleetEventServiceError -> 2
        FleetEventType' name -> (error . showText) $ "Unknown FleetEventType: " <> original name

-- | Represents the bounds of /known/ $FleetEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetEventType where
    minBound = FleetEventFleetChange
    maxBound = FleetEventServiceError

instance Hashable     FleetEventType
instance NFData       FleetEventType
instance ToByteString FleetEventType
instance ToQuery      FleetEventType
instance ToHeader     FleetEventType

instance FromXML FleetEventType where
    parseXML = parseXMLText "FleetEventType"
