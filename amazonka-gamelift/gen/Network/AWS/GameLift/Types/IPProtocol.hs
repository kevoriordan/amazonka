{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.IPProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.IPProtocol (
  IPProtocol (
    ..
    , TCP
    , Udp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPProtocol = IPProtocol' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern TCP :: IPProtocol
pattern TCP = IPProtocol' "TCP"

pattern Udp :: IPProtocol
pattern Udp = IPProtocol' "UDP"

{-# COMPLETE
  TCP,
  Udp,
  IPProtocol' #-}

instance FromText IPProtocol where
    parser = (IPProtocol' . mk) <$> takeText

instance ToText IPProtocol where
    toText (IPProtocol' ci) = original ci

-- | Represents an enum of /known/ $IPProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPProtocol where
    toEnum i = case i of
        0 -> TCP
        1 -> Udp
        _ -> (error . showText) $ "Unknown index for IPProtocol: " <> toText i
    fromEnum x = case x of
        TCP -> 0
        Udp -> 1
        IPProtocol' name -> (error . showText) $ "Unknown IPProtocol: " <> original name

-- | Represents the bounds of /known/ $IPProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPProtocol where
    minBound = TCP
    maxBound = Udp

instance Hashable     IPProtocol
instance NFData       IPProtocol
instance ToByteString IPProtocol
instance ToQuery      IPProtocol
instance ToHeader     IPProtocol

instance ToJSON IPProtocol where
    toJSON = toJSONText

instance FromJSON IPProtocol where
    parseJSON = parseJSONText "IPProtocol"
