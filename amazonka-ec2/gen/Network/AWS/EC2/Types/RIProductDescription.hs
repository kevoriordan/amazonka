{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RIProductDescription (
  RIProductDescription (
    ..
    , RIDLinuxUnix
    , RIDLinuxUnixAmazonVPC
    , RIDWindows
    , RIDWindowsAmazonVPC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RIProductDescription = RIProductDescription' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern RIDLinuxUnix :: RIProductDescription
pattern RIDLinuxUnix = RIProductDescription' "Linux/UNIX"

pattern RIDLinuxUnixAmazonVPC :: RIProductDescription
pattern RIDLinuxUnixAmazonVPC = RIProductDescription' "Linux/UNIX (Amazon VPC)"

pattern RIDWindows :: RIProductDescription
pattern RIDWindows = RIProductDescription' "Windows"

pattern RIDWindowsAmazonVPC :: RIProductDescription
pattern RIDWindowsAmazonVPC = RIProductDescription' "Windows (Amazon VPC)"

{-# COMPLETE
  RIDLinuxUnix,
  RIDLinuxUnixAmazonVPC,
  RIDWindows,
  RIDWindowsAmazonVPC,
  RIProductDescription' #-}

instance FromText RIProductDescription where
    parser = (RIProductDescription' . mk) <$> takeText

instance ToText RIProductDescription where
    toText (RIProductDescription' ci) = original ci

-- | Represents an enum of /known/ $RIProductDescription.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RIProductDescription where
    toEnum i = case i of
        0 -> RIDLinuxUnix
        1 -> RIDLinuxUnixAmazonVPC
        2 -> RIDWindows
        3 -> RIDWindowsAmazonVPC
        _ -> (error . showText) $ "Unknown index for RIProductDescription: " <> toText i
    fromEnum x = case x of
        RIDLinuxUnix -> 0
        RIDLinuxUnixAmazonVPC -> 1
        RIDWindows -> 2
        RIDWindowsAmazonVPC -> 3
        RIProductDescription' name -> (error . showText) $ "Unknown RIProductDescription: " <> original name

-- | Represents the bounds of /known/ $RIProductDescription.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RIProductDescription where
    minBound = RIDLinuxUnix
    maxBound = RIDWindowsAmazonVPC

instance Hashable     RIProductDescription
instance NFData       RIProductDescription
instance ToByteString RIProductDescription
instance ToQuery      RIProductDescription
instance ToHeader     RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"
