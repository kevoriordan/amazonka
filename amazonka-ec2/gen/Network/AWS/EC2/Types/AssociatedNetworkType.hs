{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedNetworkType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AssociatedNetworkType (
  AssociatedNetworkType (
    ..
    , ANTVPC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AssociatedNetworkType = AssociatedNetworkType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ANTVPC :: AssociatedNetworkType
pattern ANTVPC = AssociatedNetworkType' "vpc"

{-# COMPLETE
  ANTVPC,
  AssociatedNetworkType' #-}

instance FromText AssociatedNetworkType where
    parser = (AssociatedNetworkType' . mk) <$> takeText

instance ToText AssociatedNetworkType where
    toText (AssociatedNetworkType' ci) = original ci

-- | Represents an enum of /known/ $AssociatedNetworkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociatedNetworkType where
    toEnum i = case i of
        0 -> ANTVPC
        _ -> (error . showText) $ "Unknown index for AssociatedNetworkType: " <> toText i
    fromEnum x = case x of
        ANTVPC -> 0
        AssociatedNetworkType' name -> (error . showText) $ "Unknown AssociatedNetworkType: " <> original name

-- | Represents the bounds of /known/ $AssociatedNetworkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociatedNetworkType where
    minBound = ANTVPC
    maxBound = ANTVPC

instance Hashable     AssociatedNetworkType
instance NFData       AssociatedNetworkType
instance ToByteString AssociatedNetworkType
instance ToQuery      AssociatedNetworkType
instance ToHeader     AssociatedNetworkType

instance FromXML AssociatedNetworkType where
    parseXML = parseXMLText "AssociatedNetworkType"
