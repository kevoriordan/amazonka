{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MembershipType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MembershipType (
  MembershipType (
    ..
    , MTIgmp
    , MTStatic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MembershipType = MembershipType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern MTIgmp :: MembershipType
pattern MTIgmp = MembershipType' "igmp"

pattern MTStatic :: MembershipType
pattern MTStatic = MembershipType' "static"

{-# COMPLETE
  MTIgmp,
  MTStatic,
  MembershipType' #-}

instance FromText MembershipType where
    parser = (MembershipType' . mk) <$> takeText

instance ToText MembershipType where
    toText (MembershipType' ci) = original ci

-- | Represents an enum of /known/ $MembershipType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MembershipType where
    toEnum i = case i of
        0 -> MTIgmp
        1 -> MTStatic
        _ -> (error . showText) $ "Unknown index for MembershipType: " <> toText i
    fromEnum x = case x of
        MTIgmp -> 0
        MTStatic -> 1
        MembershipType' name -> (error . showText) $ "Unknown MembershipType: " <> original name

-- | Represents the bounds of /known/ $MembershipType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MembershipType where
    minBound = MTIgmp
    maxBound = MTStatic

instance Hashable     MembershipType
instance NFData       MembershipType
instance ToByteString MembershipType
instance ToQuery      MembershipType
instance ToHeader     MembershipType

instance FromXML MembershipType where
    parseXML = parseXMLText "MembershipType"
