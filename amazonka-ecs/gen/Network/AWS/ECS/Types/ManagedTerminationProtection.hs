{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedTerminationProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ManagedTerminationProtection (
  ManagedTerminationProtection (
    ..
    , MTPDisabled
    , MTPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ManagedTerminationProtection = ManagedTerminationProtection' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern MTPDisabled :: ManagedTerminationProtection
pattern MTPDisabled = ManagedTerminationProtection' "DISABLED"

pattern MTPEnabled :: ManagedTerminationProtection
pattern MTPEnabled = ManagedTerminationProtection' "ENABLED"

{-# COMPLETE
  MTPDisabled,
  MTPEnabled,
  ManagedTerminationProtection' #-}

instance FromText ManagedTerminationProtection where
    parser = (ManagedTerminationProtection' . mk) <$> takeText

instance ToText ManagedTerminationProtection where
    toText (ManagedTerminationProtection' ci) = original ci

-- | Represents an enum of /known/ $ManagedTerminationProtection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ManagedTerminationProtection where
    toEnum i = case i of
        0 -> MTPDisabled
        1 -> MTPEnabled
        _ -> (error . showText) $ "Unknown index for ManagedTerminationProtection: " <> toText i
    fromEnum x = case x of
        MTPDisabled -> 0
        MTPEnabled -> 1
        ManagedTerminationProtection' name -> (error . showText) $ "Unknown ManagedTerminationProtection: " <> original name

-- | Represents the bounds of /known/ $ManagedTerminationProtection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ManagedTerminationProtection where
    minBound = MTPDisabled
    maxBound = MTPEnabled

instance Hashable     ManagedTerminationProtection
instance NFData       ManagedTerminationProtection
instance ToByteString ManagedTerminationProtection
instance ToQuery      ManagedTerminationProtection
instance ToHeader     ManagedTerminationProtection

instance ToJSON ManagedTerminationProtection where
    toJSON = toJSONText

instance FromJSON ManagedTerminationProtection where
    parseJSON = parseJSONText "ManagedTerminationProtection"
