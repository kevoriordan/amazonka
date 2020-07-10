{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RouteTableAssociationStateCode (
  RouteTableAssociationStateCode (
    ..
    , RTASCAssociated
    , RTASCAssociating
    , RTASCDisassociated
    , RTASCDisassociating
    , RTASCFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RouteTableAssociationStateCode = RouteTableAssociationStateCode' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern RTASCAssociated :: RouteTableAssociationStateCode
pattern RTASCAssociated = RouteTableAssociationStateCode' "associated"

pattern RTASCAssociating :: RouteTableAssociationStateCode
pattern RTASCAssociating = RouteTableAssociationStateCode' "associating"

pattern RTASCDisassociated :: RouteTableAssociationStateCode
pattern RTASCDisassociated = RouteTableAssociationStateCode' "disassociated"

pattern RTASCDisassociating :: RouteTableAssociationStateCode
pattern RTASCDisassociating = RouteTableAssociationStateCode' "disassociating"

pattern RTASCFailed :: RouteTableAssociationStateCode
pattern RTASCFailed = RouteTableAssociationStateCode' "failed"

{-# COMPLETE
  RTASCAssociated,
  RTASCAssociating,
  RTASCDisassociated,
  RTASCDisassociating,
  RTASCFailed,
  RouteTableAssociationStateCode' #-}

instance FromText RouteTableAssociationStateCode where
    parser = (RouteTableAssociationStateCode' . mk) <$> takeText

instance ToText RouteTableAssociationStateCode where
    toText (RouteTableAssociationStateCode' ci) = original ci

-- | Represents an enum of /known/ $RouteTableAssociationStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RouteTableAssociationStateCode where
    toEnum i = case i of
        0 -> RTASCAssociated
        1 -> RTASCAssociating
        2 -> RTASCDisassociated
        3 -> RTASCDisassociating
        4 -> RTASCFailed
        _ -> (error . showText) $ "Unknown index for RouteTableAssociationStateCode: " <> toText i
    fromEnum x = case x of
        RTASCAssociated -> 0
        RTASCAssociating -> 1
        RTASCDisassociated -> 2
        RTASCDisassociating -> 3
        RTASCFailed -> 4
        RouteTableAssociationStateCode' name -> (error . showText) $ "Unknown RouteTableAssociationStateCode: " <> original name

-- | Represents the bounds of /known/ $RouteTableAssociationStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RouteTableAssociationStateCode where
    minBound = RTASCAssociated
    maxBound = RTASCFailed

instance Hashable     RouteTableAssociationStateCode
instance NFData       RouteTableAssociationStateCode
instance ToByteString RouteTableAssociationStateCode
instance ToQuery      RouteTableAssociationStateCode
instance ToHeader     RouteTableAssociationStateCode

instance FromXML RouteTableAssociationStateCode where
    parseXML = parseXMLText "RouteTableAssociationStateCode"
