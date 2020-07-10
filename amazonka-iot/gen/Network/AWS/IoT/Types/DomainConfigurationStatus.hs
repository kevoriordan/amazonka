{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DomainConfigurationStatus (
  DomainConfigurationStatus (
    ..
    , DCSDisabled
    , DCSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DomainConfigurationStatus = DomainConfigurationStatus' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern DCSDisabled :: DomainConfigurationStatus
pattern DCSDisabled = DomainConfigurationStatus' "DISABLED"

pattern DCSEnabled :: DomainConfigurationStatus
pattern DCSEnabled = DomainConfigurationStatus' "ENABLED"

{-# COMPLETE
  DCSDisabled,
  DCSEnabled,
  DomainConfigurationStatus' #-}

instance FromText DomainConfigurationStatus where
    parser = (DomainConfigurationStatus' . mk) <$> takeText

instance ToText DomainConfigurationStatus where
    toText (DomainConfigurationStatus' ci) = original ci

-- | Represents an enum of /known/ $DomainConfigurationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainConfigurationStatus where
    toEnum i = case i of
        0 -> DCSDisabled
        1 -> DCSEnabled
        _ -> (error . showText) $ "Unknown index for DomainConfigurationStatus: " <> toText i
    fromEnum x = case x of
        DCSDisabled -> 0
        DCSEnabled -> 1
        DomainConfigurationStatus' name -> (error . showText) $ "Unknown DomainConfigurationStatus: " <> original name

-- | Represents the bounds of /known/ $DomainConfigurationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainConfigurationStatus where
    minBound = DCSDisabled
    maxBound = DCSEnabled

instance Hashable     DomainConfigurationStatus
instance NFData       DomainConfigurationStatus
instance ToByteString DomainConfigurationStatus
instance ToQuery      DomainConfigurationStatus
instance ToHeader     DomainConfigurationStatus

instance ToJSON DomainConfigurationStatus where
    toJSON = toJSONText

instance FromJSON DomainConfigurationStatus where
    parseJSON = parseJSONText "DomainConfigurationStatus"
