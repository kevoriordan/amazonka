{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MonitoringState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MonitoringState (
  MonitoringState (
    ..
    , MSDisabled
    , MSDisabling
    , MSEnabled
    , MSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MonitoringState = MonitoringState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MSDisabled :: MonitoringState
pattern MSDisabled = MonitoringState' "disabled"

pattern MSDisabling :: MonitoringState
pattern MSDisabling = MonitoringState' "disabling"

pattern MSEnabled :: MonitoringState
pattern MSEnabled = MonitoringState' "enabled"

pattern MSPending :: MonitoringState
pattern MSPending = MonitoringState' "pending"

{-# COMPLETE
  MSDisabled,
  MSDisabling,
  MSEnabled,
  MSPending,
  MonitoringState' #-}

instance FromText MonitoringState where
    parser = (MonitoringState' . mk) <$> takeText

instance ToText MonitoringState where
    toText (MonitoringState' ci) = original ci

-- | Represents an enum of /known/ $MonitoringState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MonitoringState where
    toEnum i = case i of
        0 -> MSDisabled
        1 -> MSDisabling
        2 -> MSEnabled
        3 -> MSPending
        _ -> (error . showText) $ "Unknown index for MonitoringState: " <> toText i
    fromEnum x = case x of
        MSDisabled -> 0
        MSDisabling -> 1
        MSEnabled -> 2
        MSPending -> 3
        MonitoringState' name -> (error . showText) $ "Unknown MonitoringState: " <> original name

-- | Represents the bounds of /known/ $MonitoringState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MonitoringState where
    minBound = MSDisabled
    maxBound = MSPending

instance Hashable     MonitoringState
instance NFData       MonitoringState
instance ToByteString MonitoringState
instance ToQuery      MonitoringState
instance ToHeader     MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"
