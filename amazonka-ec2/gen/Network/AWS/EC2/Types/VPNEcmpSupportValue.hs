{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNEcmpSupportValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPNEcmpSupportValue (
  VPNEcmpSupportValue (
    ..
    , VESVDisable
    , VESVEnable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNEcmpSupportValue = VPNEcmpSupportValue' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern VESVDisable :: VPNEcmpSupportValue
pattern VESVDisable = VPNEcmpSupportValue' "disable"

pattern VESVEnable :: VPNEcmpSupportValue
pattern VESVEnable = VPNEcmpSupportValue' "enable"

{-# COMPLETE
  VESVDisable,
  VESVEnable,
  VPNEcmpSupportValue' #-}

instance FromText VPNEcmpSupportValue where
    parser = (VPNEcmpSupportValue' . mk) <$> takeText

instance ToText VPNEcmpSupportValue where
    toText (VPNEcmpSupportValue' ci) = original ci

-- | Represents an enum of /known/ $VPNEcmpSupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPNEcmpSupportValue where
    toEnum i = case i of
        0 -> VESVDisable
        1 -> VESVEnable
        _ -> (error . showText) $ "Unknown index for VPNEcmpSupportValue: " <> toText i
    fromEnum x = case x of
        VESVDisable -> 0
        VESVEnable -> 1
        VPNEcmpSupportValue' name -> (error . showText) $ "Unknown VPNEcmpSupportValue: " <> original name

-- | Represents the bounds of /known/ $VPNEcmpSupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPNEcmpSupportValue where
    minBound = VESVDisable
    maxBound = VESVEnable

instance Hashable     VPNEcmpSupportValue
instance NFData       VPNEcmpSupportValue
instance ToByteString VPNEcmpSupportValue
instance ToQuery      VPNEcmpSupportValue
instance ToHeader     VPNEcmpSupportValue

instance FromXML VPNEcmpSupportValue where
    parseXML = parseXMLText "VPNEcmpSupportValue"
