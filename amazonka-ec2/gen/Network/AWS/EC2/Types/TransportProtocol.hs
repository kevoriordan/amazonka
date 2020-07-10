{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransportProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransportProtocol (
  TransportProtocol (
    ..
    , TCP
    , Udp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransportProtocol = TransportProtocol' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern TCP :: TransportProtocol
pattern TCP = TransportProtocol' "tcp"

pattern Udp :: TransportProtocol
pattern Udp = TransportProtocol' "udp"

{-# COMPLETE
  TCP,
  Udp,
  TransportProtocol' #-}

instance FromText TransportProtocol where
    parser = (TransportProtocol' . mk) <$> takeText

instance ToText TransportProtocol where
    toText (TransportProtocol' ci) = original ci

-- | Represents an enum of /known/ $TransportProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransportProtocol where
    toEnum i = case i of
        0 -> TCP
        1 -> Udp
        _ -> (error . showText) $ "Unknown index for TransportProtocol: " <> toText i
    fromEnum x = case x of
        TCP -> 0
        Udp -> 1
        TransportProtocol' name -> (error . showText) $ "Unknown TransportProtocol: " <> original name

-- | Represents the bounds of /known/ $TransportProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransportProtocol where
    minBound = TCP
    maxBound = Udp

instance Hashable     TransportProtocol
instance NFData       TransportProtocol
instance ToByteString TransportProtocol
instance ToQuery      TransportProtocol
instance ToHeader     TransportProtocol

instance FromXML TransportProtocol where
    parseXML = parseXMLText "TransportProtocol"
