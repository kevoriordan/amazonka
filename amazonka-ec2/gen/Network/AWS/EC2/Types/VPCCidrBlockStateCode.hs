{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCCidrBlockStateCode (
  VPCCidrBlockStateCode (
    ..
    , VCBSCAssociated
    , VCBSCAssociating
    , VCBSCDisassociated
    , VCBSCDisassociating
    , VCBSCFailed
    , VCBSCFailing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCCidrBlockStateCode = VPCCidrBlockStateCode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern VCBSCAssociated :: VPCCidrBlockStateCode
pattern VCBSCAssociated = VPCCidrBlockStateCode' "associated"

pattern VCBSCAssociating :: VPCCidrBlockStateCode
pattern VCBSCAssociating = VPCCidrBlockStateCode' "associating"

pattern VCBSCDisassociated :: VPCCidrBlockStateCode
pattern VCBSCDisassociated = VPCCidrBlockStateCode' "disassociated"

pattern VCBSCDisassociating :: VPCCidrBlockStateCode
pattern VCBSCDisassociating = VPCCidrBlockStateCode' "disassociating"

pattern VCBSCFailed :: VPCCidrBlockStateCode
pattern VCBSCFailed = VPCCidrBlockStateCode' "failed"

pattern VCBSCFailing :: VPCCidrBlockStateCode
pattern VCBSCFailing = VPCCidrBlockStateCode' "failing"

{-# COMPLETE
  VCBSCAssociated,
  VCBSCAssociating,
  VCBSCDisassociated,
  VCBSCDisassociating,
  VCBSCFailed,
  VCBSCFailing,
  VPCCidrBlockStateCode' #-}

instance FromText VPCCidrBlockStateCode where
    parser = (VPCCidrBlockStateCode' . mk) <$> takeText

instance ToText VPCCidrBlockStateCode where
    toText (VPCCidrBlockStateCode' ci) = original ci

-- | Represents an enum of /known/ $VPCCidrBlockStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCCidrBlockStateCode where
    toEnum i = case i of
        0 -> VCBSCAssociated
        1 -> VCBSCAssociating
        2 -> VCBSCDisassociated
        3 -> VCBSCDisassociating
        4 -> VCBSCFailed
        5 -> VCBSCFailing
        _ -> (error . showText) $ "Unknown index for VPCCidrBlockStateCode: " <> toText i
    fromEnum x = case x of
        VCBSCAssociated -> 0
        VCBSCAssociating -> 1
        VCBSCDisassociated -> 2
        VCBSCDisassociating -> 3
        VCBSCFailed -> 4
        VCBSCFailing -> 5
        VPCCidrBlockStateCode' name -> (error . showText) $ "Unknown VPCCidrBlockStateCode: " <> original name

-- | Represents the bounds of /known/ $VPCCidrBlockStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCCidrBlockStateCode where
    minBound = VCBSCAssociated
    maxBound = VCBSCFailing

instance Hashable     VPCCidrBlockStateCode
instance NFData       VPCCidrBlockStateCode
instance ToByteString VPCCidrBlockStateCode
instance ToQuery      VPCCidrBlockStateCode
instance ToHeader     VPCCidrBlockStateCode

instance FromXML VPCCidrBlockStateCode where
    parseXML = parseXMLText "VPCCidrBlockStateCode"
