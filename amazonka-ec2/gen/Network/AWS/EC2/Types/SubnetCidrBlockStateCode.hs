{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SubnetCidrBlockStateCode (
  SubnetCidrBlockStateCode (
    ..
    , SCBSCAssociated
    , SCBSCAssociating
    , SCBSCDisassociated
    , SCBSCDisassociating
    , SCBSCFailed
    , SCBSCFailing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SubnetCidrBlockStateCode = SubnetCidrBlockStateCode' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern SCBSCAssociated :: SubnetCidrBlockStateCode
pattern SCBSCAssociated = SubnetCidrBlockStateCode' "associated"

pattern SCBSCAssociating :: SubnetCidrBlockStateCode
pattern SCBSCAssociating = SubnetCidrBlockStateCode' "associating"

pattern SCBSCDisassociated :: SubnetCidrBlockStateCode
pattern SCBSCDisassociated = SubnetCidrBlockStateCode' "disassociated"

pattern SCBSCDisassociating :: SubnetCidrBlockStateCode
pattern SCBSCDisassociating = SubnetCidrBlockStateCode' "disassociating"

pattern SCBSCFailed :: SubnetCidrBlockStateCode
pattern SCBSCFailed = SubnetCidrBlockStateCode' "failed"

pattern SCBSCFailing :: SubnetCidrBlockStateCode
pattern SCBSCFailing = SubnetCidrBlockStateCode' "failing"

{-# COMPLETE
  SCBSCAssociated,
  SCBSCAssociating,
  SCBSCDisassociated,
  SCBSCDisassociating,
  SCBSCFailed,
  SCBSCFailing,
  SubnetCidrBlockStateCode' #-}

instance FromText SubnetCidrBlockStateCode where
    parser = (SubnetCidrBlockStateCode' . mk) <$> takeText

instance ToText SubnetCidrBlockStateCode where
    toText (SubnetCidrBlockStateCode' ci) = original ci

-- | Represents an enum of /known/ $SubnetCidrBlockStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubnetCidrBlockStateCode where
    toEnum i = case i of
        0 -> SCBSCAssociated
        1 -> SCBSCAssociating
        2 -> SCBSCDisassociated
        3 -> SCBSCDisassociating
        4 -> SCBSCFailed
        5 -> SCBSCFailing
        _ -> (error . showText) $ "Unknown index for SubnetCidrBlockStateCode: " <> toText i
    fromEnum x = case x of
        SCBSCAssociated -> 0
        SCBSCAssociating -> 1
        SCBSCDisassociated -> 2
        SCBSCDisassociating -> 3
        SCBSCFailed -> 4
        SCBSCFailing -> 5
        SubnetCidrBlockStateCode' name -> (error . showText) $ "Unknown SubnetCidrBlockStateCode: " <> original name

-- | Represents the bounds of /known/ $SubnetCidrBlockStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubnetCidrBlockStateCode where
    minBound = SCBSCAssociated
    maxBound = SCBSCFailing

instance Hashable     SubnetCidrBlockStateCode
instance NFData       SubnetCidrBlockStateCode
instance ToByteString SubnetCidrBlockStateCode
instance ToQuery      SubnetCidrBlockStateCode
instance ToHeader     SubnetCidrBlockStateCode

instance FromXML SubnetCidrBlockStateCode where
    parseXML = parseXMLText "SubnetCidrBlockStateCode"
