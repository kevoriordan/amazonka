{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DefaultRouteTableAssociationValue (
  DefaultRouteTableAssociationValue (
    ..
    , DRTAVDisable
    , DRTAVEnable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DefaultRouteTableAssociationValue = DefaultRouteTableAssociationValue' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern DRTAVDisable :: DefaultRouteTableAssociationValue
pattern DRTAVDisable = DefaultRouteTableAssociationValue' "disable"

pattern DRTAVEnable :: DefaultRouteTableAssociationValue
pattern DRTAVEnable = DefaultRouteTableAssociationValue' "enable"

{-# COMPLETE
  DRTAVDisable,
  DRTAVEnable,
  DefaultRouteTableAssociationValue' #-}

instance FromText DefaultRouteTableAssociationValue where
    parser = (DefaultRouteTableAssociationValue' . mk) <$> takeText

instance ToText DefaultRouteTableAssociationValue where
    toText (DefaultRouteTableAssociationValue' ci) = original ci

-- | Represents an enum of /known/ $DefaultRouteTableAssociationValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DefaultRouteTableAssociationValue where
    toEnum i = case i of
        0 -> DRTAVDisable
        1 -> DRTAVEnable
        _ -> (error . showText) $ "Unknown index for DefaultRouteTableAssociationValue: " <> toText i
    fromEnum x = case x of
        DRTAVDisable -> 0
        DRTAVEnable -> 1
        DefaultRouteTableAssociationValue' name -> (error . showText) $ "Unknown DefaultRouteTableAssociationValue: " <> original name

-- | Represents the bounds of /known/ $DefaultRouteTableAssociationValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DefaultRouteTableAssociationValue where
    minBound = DRTAVDisable
    maxBound = DRTAVEnable

instance Hashable     DefaultRouteTableAssociationValue
instance NFData       DefaultRouteTableAssociationValue
instance ToByteString DefaultRouteTableAssociationValue
instance ToQuery      DefaultRouteTableAssociationValue
instance ToHeader     DefaultRouteTableAssociationValue

instance FromXML DefaultRouteTableAssociationValue where
    parseXML = parseXMLText "DefaultRouteTableAssociationValue"
