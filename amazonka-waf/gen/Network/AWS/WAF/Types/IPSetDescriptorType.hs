{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetDescriptorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.IPSetDescriptorType (
  IPSetDescriptorType (
    ..
    , IPV4
    , IPV6
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPSetDescriptorType = IPSetDescriptorType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern IPV4 :: IPSetDescriptorType
pattern IPV4 = IPSetDescriptorType' "IPV4"

pattern IPV6 :: IPSetDescriptorType
pattern IPV6 = IPSetDescriptorType' "IPV6"

{-# COMPLETE
  IPV4,
  IPV6,
  IPSetDescriptorType' #-}

instance FromText IPSetDescriptorType where
    parser = (IPSetDescriptorType' . mk) <$> takeText

instance ToText IPSetDescriptorType where
    toText (IPSetDescriptorType' ci) = original ci

-- | Represents an enum of /known/ $IPSetDescriptorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPSetDescriptorType where
    toEnum i = case i of
        0 -> IPV4
        1 -> IPV6
        _ -> (error . showText) $ "Unknown index for IPSetDescriptorType: " <> toText i
    fromEnum x = case x of
        IPV4 -> 0
        IPV6 -> 1
        IPSetDescriptorType' name -> (error . showText) $ "Unknown IPSetDescriptorType: " <> original name

-- | Represents the bounds of /known/ $IPSetDescriptorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPSetDescriptorType where
    minBound = IPV4
    maxBound = IPV6

instance Hashable     IPSetDescriptorType
instance NFData       IPSetDescriptorType
instance ToByteString IPSetDescriptorType
instance ToQuery      IPSetDescriptorType
instance ToHeader     IPSetDescriptorType

instance ToJSON IPSetDescriptorType where
    toJSON = toJSONText

instance FromJSON IPSetDescriptorType where
    parseJSON = parseJSONText "IPSetDescriptorType"
