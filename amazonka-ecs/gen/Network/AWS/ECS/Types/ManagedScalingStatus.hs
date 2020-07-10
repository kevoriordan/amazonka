{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedScalingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ManagedScalingStatus (
  ManagedScalingStatus (
    ..
    , MSSDisabled
    , MSSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ManagedScalingStatus = ManagedScalingStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern MSSDisabled :: ManagedScalingStatus
pattern MSSDisabled = ManagedScalingStatus' "DISABLED"

pattern MSSEnabled :: ManagedScalingStatus
pattern MSSEnabled = ManagedScalingStatus' "ENABLED"

{-# COMPLETE
  MSSDisabled,
  MSSEnabled,
  ManagedScalingStatus' #-}

instance FromText ManagedScalingStatus where
    parser = (ManagedScalingStatus' . mk) <$> takeText

instance ToText ManagedScalingStatus where
    toText (ManagedScalingStatus' ci) = original ci

-- | Represents an enum of /known/ $ManagedScalingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ManagedScalingStatus where
    toEnum i = case i of
        0 -> MSSDisabled
        1 -> MSSEnabled
        _ -> (error . showText) $ "Unknown index for ManagedScalingStatus: " <> toText i
    fromEnum x = case x of
        MSSDisabled -> 0
        MSSEnabled -> 1
        ManagedScalingStatus' name -> (error . showText) $ "Unknown ManagedScalingStatus: " <> original name

-- | Represents the bounds of /known/ $ManagedScalingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ManagedScalingStatus where
    minBound = MSSDisabled
    maxBound = MSSEnabled

instance Hashable     ManagedScalingStatus
instance NFData       ManagedScalingStatus
instance ToByteString ManagedScalingStatus
instance ToQuery      ManagedScalingStatus
instance ToHeader     ManagedScalingStatus

instance ToJSON ManagedScalingStatus where
    toJSON = toJSONText

instance FromJSON ManagedScalingStatus where
    parseJSON = parseJSONText "ManagedScalingStatus"
