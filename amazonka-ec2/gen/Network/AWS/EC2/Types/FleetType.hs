{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetType (
  FleetType (
    ..
    , FleetInstant
    , FleetMaintain
    , FleetRequest
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetType = FleetType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern FleetInstant :: FleetType
pattern FleetInstant = FleetType' "instant"

pattern FleetMaintain :: FleetType
pattern FleetMaintain = FleetType' "maintain"

pattern FleetRequest :: FleetType
pattern FleetRequest = FleetType' "request"

{-# COMPLETE
  FleetInstant,
  FleetMaintain,
  FleetRequest,
  FleetType' #-}

instance FromText FleetType where
    parser = (FleetType' . mk) <$> takeText

instance ToText FleetType where
    toText (FleetType' ci) = original ci

-- | Represents an enum of /known/ $FleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetType where
    toEnum i = case i of
        0 -> FleetInstant
        1 -> FleetMaintain
        2 -> FleetRequest
        _ -> (error . showText) $ "Unknown index for FleetType: " <> toText i
    fromEnum x = case x of
        FleetInstant -> 0
        FleetMaintain -> 1
        FleetRequest -> 2
        FleetType' name -> (error . showText) $ "Unknown FleetType: " <> original name

-- | Represents the bounds of /known/ $FleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetType where
    minBound = FleetInstant
    maxBound = FleetRequest

instance Hashable     FleetType
instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance FromXML FleetType where
    parseXML = parseXMLText "FleetType"
