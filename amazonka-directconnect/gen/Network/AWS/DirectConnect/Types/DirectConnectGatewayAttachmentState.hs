{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState (
  DirectConnectGatewayAttachmentState (
    ..
    , Attached
    , Attaching
    , Detached
    , Detaching
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern Attached :: DirectConnectGatewayAttachmentState
pattern Attached = DirectConnectGatewayAttachmentState' "attached"

pattern Attaching :: DirectConnectGatewayAttachmentState
pattern Attaching = DirectConnectGatewayAttachmentState' "attaching"

pattern Detached :: DirectConnectGatewayAttachmentState
pattern Detached = DirectConnectGatewayAttachmentState' "detached"

pattern Detaching :: DirectConnectGatewayAttachmentState
pattern Detaching = DirectConnectGatewayAttachmentState' "detaching"

{-# COMPLETE
  Attached,
  Attaching,
  Detached,
  Detaching,
  DirectConnectGatewayAttachmentState' #-}

instance FromText DirectConnectGatewayAttachmentState where
    parser = (DirectConnectGatewayAttachmentState' . mk) <$> takeText

instance ToText DirectConnectGatewayAttachmentState where
    toText (DirectConnectGatewayAttachmentState' ci) = original ci

-- | Represents an enum of /known/ $DirectConnectGatewayAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectConnectGatewayAttachmentState where
    toEnum i = case i of
        0 -> Attached
        1 -> Attaching
        2 -> Detached
        3 -> Detaching
        _ -> (error . showText) $ "Unknown index for DirectConnectGatewayAttachmentState: " <> toText i
    fromEnum x = case x of
        Attached -> 0
        Attaching -> 1
        Detached -> 2
        Detaching -> 3
        DirectConnectGatewayAttachmentState' name -> (error . showText) $ "Unknown DirectConnectGatewayAttachmentState: " <> original name

-- | Represents the bounds of /known/ $DirectConnectGatewayAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectConnectGatewayAttachmentState where
    minBound = Attached
    maxBound = Detaching

instance Hashable     DirectConnectGatewayAttachmentState
instance NFData       DirectConnectGatewayAttachmentState
instance ToByteString DirectConnectGatewayAttachmentState
instance ToQuery      DirectConnectGatewayAttachmentState
instance ToHeader     DirectConnectGatewayAttachmentState

instance FromJSON DirectConnectGatewayAttachmentState where
    parseJSON = parseJSONText "DirectConnectGatewayAttachmentState"
