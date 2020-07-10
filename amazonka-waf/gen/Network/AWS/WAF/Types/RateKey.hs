{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RateKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.RateKey (
  RateKey (
    ..
    , IP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RateKey = RateKey' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern IP :: RateKey
pattern IP = RateKey' "IP"

{-# COMPLETE
  IP,
  RateKey' #-}

instance FromText RateKey where
    parser = (RateKey' . mk) <$> takeText

instance ToText RateKey where
    toText (RateKey' ci) = original ci

-- | Represents an enum of /known/ $RateKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RateKey where
    toEnum i = case i of
        0 -> IP
        _ -> (error . showText) $ "Unknown index for RateKey: " <> toText i
    fromEnum x = case x of
        IP -> 0
        RateKey' name -> (error . showText) $ "Unknown RateKey: " <> original name

-- | Represents the bounds of /known/ $RateKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RateKey where
    minBound = IP
    maxBound = IP

instance Hashable     RateKey
instance NFData       RateKey
instance ToByteString RateKey
instance ToQuery      RateKey
instance ToHeader     RateKey

instance ToJSON RateKey where
    toJSON = toJSONText

instance FromJSON RateKey where
    parseJSON = parseJSONText "RateKey"
