{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayPropagationState (
  TransitGatewayPropagationState (
    ..
    , Disabled
    , Disabling
    , Enabled
    , Enabling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayPropagationState = TransitGatewayPropagationState' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern Disabled :: TransitGatewayPropagationState
pattern Disabled = TransitGatewayPropagationState' "disabled"

pattern Disabling :: TransitGatewayPropagationState
pattern Disabling = TransitGatewayPropagationState' "disabling"

pattern Enabled :: TransitGatewayPropagationState
pattern Enabled = TransitGatewayPropagationState' "enabled"

pattern Enabling :: TransitGatewayPropagationState
pattern Enabling = TransitGatewayPropagationState' "enabling"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
  TransitGatewayPropagationState' #-}

instance FromText TransitGatewayPropagationState where
    parser = (TransitGatewayPropagationState' . mk) <$> takeText

instance ToText TransitGatewayPropagationState where
    toText (TransitGatewayPropagationState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayPropagationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayPropagationState where
    toEnum i = case i of
        0 -> Disabled
        1 -> Disabling
        2 -> Enabled
        3 -> Enabling
        _ -> (error . showText) $ "Unknown index for TransitGatewayPropagationState: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Disabling -> 1
        Enabled -> 2
        Enabling -> 3
        TransitGatewayPropagationState' name -> (error . showText) $ "Unknown TransitGatewayPropagationState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayPropagationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayPropagationState where
    minBound = Disabled
    maxBound = Enabling

instance Hashable     TransitGatewayPropagationState
instance NFData       TransitGatewayPropagationState
instance ToByteString TransitGatewayPropagationState
instance ToQuery      TransitGatewayPropagationState
instance ToHeader     TransitGatewayPropagationState

instance FromXML TransitGatewayPropagationState where
    parseXML = parseXMLText "TransitGatewayPropagationState"
