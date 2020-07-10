{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus (
  ModifyAvailabilityZoneOptInStatus (
    ..
    , NotOptedIn
    , OptedIn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ModifyAvailabilityZoneOptInStatus = ModifyAvailabilityZoneOptInStatus' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern NotOptedIn :: ModifyAvailabilityZoneOptInStatus
pattern NotOptedIn = ModifyAvailabilityZoneOptInStatus' "not-opted-in"

pattern OptedIn :: ModifyAvailabilityZoneOptInStatus
pattern OptedIn = ModifyAvailabilityZoneOptInStatus' "opted-in"

{-# COMPLETE
  NotOptedIn,
  OptedIn,
  ModifyAvailabilityZoneOptInStatus' #-}

instance FromText ModifyAvailabilityZoneOptInStatus where
    parser = (ModifyAvailabilityZoneOptInStatus' . mk) <$> takeText

instance ToText ModifyAvailabilityZoneOptInStatus where
    toText (ModifyAvailabilityZoneOptInStatus' ci) = original ci

-- | Represents an enum of /known/ $ModifyAvailabilityZoneOptInStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModifyAvailabilityZoneOptInStatus where
    toEnum i = case i of
        0 -> NotOptedIn
        1 -> OptedIn
        _ -> (error . showText) $ "Unknown index for ModifyAvailabilityZoneOptInStatus: " <> toText i
    fromEnum x = case x of
        NotOptedIn -> 0
        OptedIn -> 1
        ModifyAvailabilityZoneOptInStatus' name -> (error . showText) $ "Unknown ModifyAvailabilityZoneOptInStatus: " <> original name

-- | Represents the bounds of /known/ $ModifyAvailabilityZoneOptInStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModifyAvailabilityZoneOptInStatus where
    minBound = NotOptedIn
    maxBound = OptedIn

instance Hashable     ModifyAvailabilityZoneOptInStatus
instance NFData       ModifyAvailabilityZoneOptInStatus
instance ToByteString ModifyAvailabilityZoneOptInStatus
instance ToQuery      ModifyAvailabilityZoneOptInStatus
instance ToHeader     ModifyAvailabilityZoneOptInStatus
