{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.JobState (
  JobState (
    ..
    , JSCancelled
    , JSComplete
    , JSInProgress
    , JSInTransitToAWS
    , JSInTransitToCustomer
    , JSListing
    , JSNew
    , JSPending
    , JSPreparingAppliance
    , JSPreparingShipment
    , JSWithAWS
    , JSWithAWSSortingFacility
    , JSWithCustomer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobState = JobState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern JSCancelled :: JobState
pattern JSCancelled = JobState' "Cancelled"

pattern JSComplete :: JobState
pattern JSComplete = JobState' "Complete"

pattern JSInProgress :: JobState
pattern JSInProgress = JobState' "InProgress"

pattern JSInTransitToAWS :: JobState
pattern JSInTransitToAWS = JobState' "InTransitToAWS"

pattern JSInTransitToCustomer :: JobState
pattern JSInTransitToCustomer = JobState' "InTransitToCustomer"

pattern JSListing :: JobState
pattern JSListing = JobState' "Listing"

pattern JSNew :: JobState
pattern JSNew = JobState' "New"

pattern JSPending :: JobState
pattern JSPending = JobState' "Pending"

pattern JSPreparingAppliance :: JobState
pattern JSPreparingAppliance = JobState' "PreparingAppliance"

pattern JSPreparingShipment :: JobState
pattern JSPreparingShipment = JobState' "PreparingShipment"

pattern JSWithAWS :: JobState
pattern JSWithAWS = JobState' "WithAWS"

pattern JSWithAWSSortingFacility :: JobState
pattern JSWithAWSSortingFacility = JobState' "WithAWSSortingFacility"

pattern JSWithCustomer :: JobState
pattern JSWithCustomer = JobState' "WithCustomer"

{-# COMPLETE
  JSCancelled,
  JSComplete,
  JSInProgress,
  JSInTransitToAWS,
  JSInTransitToCustomer,
  JSListing,
  JSNew,
  JSPending,
  JSPreparingAppliance,
  JSPreparingShipment,
  JSWithAWS,
  JSWithAWSSortingFacility,
  JSWithCustomer,
  JobState' #-}

instance FromText JobState where
    parser = (JobState' . mk) <$> takeText

instance ToText JobState where
    toText (JobState' ci) = original ci

-- | Represents an enum of /known/ $JobState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobState where
    toEnum i = case i of
        0 -> JSCancelled
        1 -> JSComplete
        2 -> JSInProgress
        3 -> JSInTransitToAWS
        4 -> JSInTransitToCustomer
        5 -> JSListing
        6 -> JSNew
        7 -> JSPending
        8 -> JSPreparingAppliance
        9 -> JSPreparingShipment
        10 -> JSWithAWS
        11 -> JSWithAWSSortingFacility
        12 -> JSWithCustomer
        _ -> (error . showText) $ "Unknown index for JobState: " <> toText i
    fromEnum x = case x of
        JSCancelled -> 0
        JSComplete -> 1
        JSInProgress -> 2
        JSInTransitToAWS -> 3
        JSInTransitToCustomer -> 4
        JSListing -> 5
        JSNew -> 6
        JSPending -> 7
        JSPreparingAppliance -> 8
        JSPreparingShipment -> 9
        JSWithAWS -> 10
        JSWithAWSSortingFacility -> 11
        JSWithCustomer -> 12
        JobState' name -> (error . showText) $ "Unknown JobState: " <> original name

-- | Represents the bounds of /known/ $JobState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobState where
    minBound = JSCancelled
    maxBound = JSWithCustomer

instance Hashable     JobState
instance NFData       JobState
instance ToByteString JobState
instance ToQuery      JobState
instance ToHeader     JobState

instance ToJSON JobState where
    toJSON = toJSONText

instance FromJSON JobState where
    parseJSON = parseJSONText "JobState"
