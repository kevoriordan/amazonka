{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceAttribute (
  NetworkInterfaceAttribute (
    ..
    , NIAAttachment
    , NIADescription
    , NIAGroupSet
    , NIASourceDestCheck
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceAttribute = NetworkInterfaceAttribute' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern NIAAttachment :: NetworkInterfaceAttribute
pattern NIAAttachment = NetworkInterfaceAttribute' "attachment"

pattern NIADescription :: NetworkInterfaceAttribute
pattern NIADescription = NetworkInterfaceAttribute' "description"

pattern NIAGroupSet :: NetworkInterfaceAttribute
pattern NIAGroupSet = NetworkInterfaceAttribute' "groupSet"

pattern NIASourceDestCheck :: NetworkInterfaceAttribute
pattern NIASourceDestCheck = NetworkInterfaceAttribute' "sourceDestCheck"

{-# COMPLETE
  NIAAttachment,
  NIADescription,
  NIAGroupSet,
  NIASourceDestCheck,
  NetworkInterfaceAttribute' #-}

instance FromText NetworkInterfaceAttribute where
    parser = (NetworkInterfaceAttribute' . mk) <$> takeText

instance ToText NetworkInterfaceAttribute where
    toText (NetworkInterfaceAttribute' ci) = original ci

-- | Represents an enum of /known/ $NetworkInterfaceAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkInterfaceAttribute where
    toEnum i = case i of
        0 -> NIAAttachment
        1 -> NIADescription
        2 -> NIAGroupSet
        3 -> NIASourceDestCheck
        _ -> (error . showText) $ "Unknown index for NetworkInterfaceAttribute: " <> toText i
    fromEnum x = case x of
        NIAAttachment -> 0
        NIADescription -> 1
        NIAGroupSet -> 2
        NIASourceDestCheck -> 3
        NetworkInterfaceAttribute' name -> (error . showText) $ "Unknown NetworkInterfaceAttribute: " <> original name

-- | Represents the bounds of /known/ $NetworkInterfaceAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkInterfaceAttribute where
    minBound = NIAAttachment
    maxBound = NIASourceDestCheck

instance Hashable     NetworkInterfaceAttribute
instance NFData       NetworkInterfaceAttribute
instance ToByteString NetworkInterfaceAttribute
instance ToQuery      NetworkInterfaceAttribute
instance ToHeader     NetworkInterfaceAttribute
