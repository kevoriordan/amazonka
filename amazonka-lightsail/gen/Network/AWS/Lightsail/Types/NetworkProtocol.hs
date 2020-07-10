{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.NetworkProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.NetworkProtocol (
  NetworkProtocol (
    ..
    , All
    , TCP
    , Udp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NetworkProtocol = NetworkProtocol' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern All :: NetworkProtocol
pattern All = NetworkProtocol' "all"

pattern TCP :: NetworkProtocol
pattern TCP = NetworkProtocol' "tcp"

pattern Udp :: NetworkProtocol
pattern Udp = NetworkProtocol' "udp"

{-# COMPLETE
  All,
  TCP,
  Udp,
  NetworkProtocol' #-}

instance FromText NetworkProtocol where
    parser = (NetworkProtocol' . mk) <$> takeText

instance ToText NetworkProtocol where
    toText (NetworkProtocol' ci) = original ci

-- | Represents an enum of /known/ $NetworkProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkProtocol where
    toEnum i = case i of
        0 -> All
        1 -> TCP
        2 -> Udp
        _ -> (error . showText) $ "Unknown index for NetworkProtocol: " <> toText i
    fromEnum x = case x of
        All -> 0
        TCP -> 1
        Udp -> 2
        NetworkProtocol' name -> (error . showText) $ "Unknown NetworkProtocol: " <> original name

-- | Represents the bounds of /known/ $NetworkProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkProtocol where
    minBound = All
    maxBound = Udp

instance Hashable     NetworkProtocol
instance NFData       NetworkProtocol
instance ToByteString NetworkProtocol
instance ToQuery      NetworkProtocol
instance ToHeader     NetworkProtocol

instance ToJSON NetworkProtocol where
    toJSON = toJSONText

instance FromJSON NetworkProtocol where
    parseJSON = parseJSONText "NetworkProtocol"
