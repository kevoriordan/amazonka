{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkEapMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.NetworkEapMethod (
  NetworkEapMethod (
    ..
    , EapTLS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NetworkEapMethod = NetworkEapMethod' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern EapTLS :: NetworkEapMethod
pattern EapTLS = NetworkEapMethod' "EAP_TLS"

{-# COMPLETE
  EapTLS,
  NetworkEapMethod' #-}

instance FromText NetworkEapMethod where
    parser = (NetworkEapMethod' . mk) <$> takeText

instance ToText NetworkEapMethod where
    toText (NetworkEapMethod' ci) = original ci

-- | Represents an enum of /known/ $NetworkEapMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkEapMethod where
    toEnum i = case i of
        0 -> EapTLS
        _ -> (error . showText) $ "Unknown index for NetworkEapMethod: " <> toText i
    fromEnum x = case x of
        EapTLS -> 0
        NetworkEapMethod' name -> (error . showText) $ "Unknown NetworkEapMethod: " <> original name

-- | Represents the bounds of /known/ $NetworkEapMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkEapMethod where
    minBound = EapTLS
    maxBound = EapTLS

instance Hashable     NetworkEapMethod
instance NFData       NetworkEapMethod
instance ToByteString NetworkEapMethod
instance ToQuery      NetworkEapMethod
instance ToHeader     NetworkEapMethod

instance ToJSON NetworkEapMethod where
    toJSON = toJSONText

instance FromJSON NetworkEapMethod where
    parseJSON = parseJSONText "NetworkEapMethod"
