{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AccessPropertyValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.AccessPropertyValue (
  AccessPropertyValue (
    ..
    , Allow
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccessPropertyValue = AccessPropertyValue' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Allow :: AccessPropertyValue
pattern Allow = AccessPropertyValue' "ALLOW"

pattern Deny :: AccessPropertyValue
pattern Deny = AccessPropertyValue' "DENY"

{-# COMPLETE
  Allow,
  Deny,
  AccessPropertyValue' #-}

instance FromText AccessPropertyValue where
    parser = (AccessPropertyValue' . mk) <$> takeText

instance ToText AccessPropertyValue where
    toText (AccessPropertyValue' ci) = original ci

-- | Represents an enum of /known/ $AccessPropertyValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccessPropertyValue where
    toEnum i = case i of
        0 -> Allow
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for AccessPropertyValue: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Deny -> 1
        AccessPropertyValue' name -> (error . showText) $ "Unknown AccessPropertyValue: " <> original name

-- | Represents the bounds of /known/ $AccessPropertyValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccessPropertyValue where
    minBound = Allow
    maxBound = Deny

instance Hashable     AccessPropertyValue
instance NFData       AccessPropertyValue
instance ToByteString AccessPropertyValue
instance ToQuery      AccessPropertyValue
instance ToHeader     AccessPropertyValue

instance ToJSON AccessPropertyValue where
    toJSON = toJSONText

instance FromJSON AccessPropertyValue where
    parseJSON = parseJSONText "AccessPropertyValue"
