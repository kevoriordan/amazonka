{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.InstanceStatus (
  InstanceStatus (
    ..
    , ISActive
    , ISPending
    , ISTerminating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceStatus = InstanceStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ISActive :: InstanceStatus
pattern ISActive = InstanceStatus' "ACTIVE"

pattern ISPending :: InstanceStatus
pattern ISPending = InstanceStatus' "PENDING"

pattern ISTerminating :: InstanceStatus
pattern ISTerminating = InstanceStatus' "TERMINATING"

{-# COMPLETE
  ISActive,
  ISPending,
  ISTerminating,
  InstanceStatus' #-}

instance FromText InstanceStatus where
    parser = (InstanceStatus' . mk) <$> takeText

instance ToText InstanceStatus where
    toText (InstanceStatus' ci) = original ci

-- | Represents an enum of /known/ $InstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceStatus where
    toEnum i = case i of
        0 -> ISActive
        1 -> ISPending
        2 -> ISTerminating
        _ -> (error . showText) $ "Unknown index for InstanceStatus: " <> toText i
    fromEnum x = case x of
        ISActive -> 0
        ISPending -> 1
        ISTerminating -> 2
        InstanceStatus' name -> (error . showText) $ "Unknown InstanceStatus: " <> original name

-- | Represents the bounds of /known/ $InstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceStatus where
    minBound = ISActive
    maxBound = ISTerminating

instance Hashable     InstanceStatus
instance NFData       InstanceStatus
instance ToByteString InstanceStatus
instance ToQuery      InstanceStatus
instance ToHeader     InstanceStatus

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"
