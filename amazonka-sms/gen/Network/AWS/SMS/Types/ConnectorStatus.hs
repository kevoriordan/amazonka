{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ConnectorStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ConnectorStatus (
  ConnectorStatus (
    ..
    , Healthy
    , Unhealthy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectorStatus = ConnectorStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Healthy :: ConnectorStatus
pattern Healthy = ConnectorStatus' "HEALTHY"

pattern Unhealthy :: ConnectorStatus
pattern Unhealthy = ConnectorStatus' "UNHEALTHY"

{-# COMPLETE
  Healthy,
  Unhealthy,
  ConnectorStatus' #-}

instance FromText ConnectorStatus where
    parser = (ConnectorStatus' . mk) <$> takeText

instance ToText ConnectorStatus where
    toText (ConnectorStatus' ci) = original ci

-- | Represents an enum of /known/ $ConnectorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectorStatus where
    toEnum i = case i of
        0 -> Healthy
        1 -> Unhealthy
        _ -> (error . showText) $ "Unknown index for ConnectorStatus: " <> toText i
    fromEnum x = case x of
        Healthy -> 0
        Unhealthy -> 1
        ConnectorStatus' name -> (error . showText) $ "Unknown ConnectorStatus: " <> original name

-- | Represents the bounds of /known/ $ConnectorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectorStatus where
    minBound = Healthy
    maxBound = Unhealthy

instance Hashable     ConnectorStatus
instance NFData       ConnectorStatus
instance ToByteString ConnectorStatus
instance ToQuery      ConnectorStatus
instance ToHeader     ConnectorStatus

instance FromJSON ConnectorStatus where
    parseJSON = parseJSONText "ConnectorStatus"
