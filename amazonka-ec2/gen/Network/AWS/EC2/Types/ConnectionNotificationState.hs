{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotificationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ConnectionNotificationState (
  ConnectionNotificationState (
    ..
    , CNSDisabled
    , CNSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConnectionNotificationState = ConnectionNotificationState' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern CNSDisabled :: ConnectionNotificationState
pattern CNSDisabled = ConnectionNotificationState' "Disabled"

pattern CNSEnabled :: ConnectionNotificationState
pattern CNSEnabled = ConnectionNotificationState' "Enabled"

{-# COMPLETE
  CNSDisabled,
  CNSEnabled,
  ConnectionNotificationState' #-}

instance FromText ConnectionNotificationState where
    parser = (ConnectionNotificationState' . mk) <$> takeText

instance ToText ConnectionNotificationState where
    toText (ConnectionNotificationState' ci) = original ci

-- | Represents an enum of /known/ $ConnectionNotificationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionNotificationState where
    toEnum i = case i of
        0 -> CNSDisabled
        1 -> CNSEnabled
        _ -> (error . showText) $ "Unknown index for ConnectionNotificationState: " <> toText i
    fromEnum x = case x of
        CNSDisabled -> 0
        CNSEnabled -> 1
        ConnectionNotificationState' name -> (error . showText) $ "Unknown ConnectionNotificationState: " <> original name

-- | Represents the bounds of /known/ $ConnectionNotificationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionNotificationState where
    minBound = CNSDisabled
    maxBound = CNSEnabled

instance Hashable     ConnectionNotificationState
instance NFData       ConnectionNotificationState
instance ToByteString ConnectionNotificationState
instance ToQuery      ConnectionNotificationState
instance ToHeader     ConnectionNotificationState

instance FromXML ConnectionNotificationState where
    parseXML = parseXMLText "ConnectionNotificationState"
