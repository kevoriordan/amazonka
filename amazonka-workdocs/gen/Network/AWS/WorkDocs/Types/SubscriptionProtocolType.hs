{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SubscriptionProtocolType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.SubscriptionProtocolType (
  SubscriptionProtocolType (
    ..
    , HTTPS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SubscriptionProtocolType = SubscriptionProtocolType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HTTPS :: SubscriptionProtocolType
pattern HTTPS = SubscriptionProtocolType' "HTTPS"

{-# COMPLETE
  HTTPS,
  SubscriptionProtocolType' #-}

instance FromText SubscriptionProtocolType where
    parser = (SubscriptionProtocolType' . mk) <$> takeText

instance ToText SubscriptionProtocolType where
    toText (SubscriptionProtocolType' ci) = original ci

-- | Represents an enum of /known/ $SubscriptionProtocolType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubscriptionProtocolType where
    toEnum i = case i of
        0 -> HTTPS
        _ -> (error . showText) $ "Unknown index for SubscriptionProtocolType: " <> toText i
    fromEnum x = case x of
        HTTPS -> 0
        SubscriptionProtocolType' name -> (error . showText) $ "Unknown SubscriptionProtocolType: " <> original name

-- | Represents the bounds of /known/ $SubscriptionProtocolType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubscriptionProtocolType where
    minBound = HTTPS
    maxBound = HTTPS

instance Hashable     SubscriptionProtocolType
instance NFData       SubscriptionProtocolType
instance ToByteString SubscriptionProtocolType
instance ToQuery      SubscriptionProtocolType
instance ToHeader     SubscriptionProtocolType

instance ToJSON SubscriptionProtocolType where
    toJSON = toJSONText

instance FromJSON SubscriptionProtocolType where
    parseJSON = parseJSONText "SubscriptionProtocolType"
