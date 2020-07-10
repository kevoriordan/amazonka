{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestrictionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.GeoRestrictionType (
  GeoRestrictionType (
    ..
    , Blacklist
    , None
    , Whitelist
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GeoRestrictionType = GeoRestrictionType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Blacklist :: GeoRestrictionType
pattern Blacklist = GeoRestrictionType' "blacklist"

pattern None :: GeoRestrictionType
pattern None = GeoRestrictionType' "none"

pattern Whitelist :: GeoRestrictionType
pattern Whitelist = GeoRestrictionType' "whitelist"

{-# COMPLETE
  Blacklist,
  None,
  Whitelist,
  GeoRestrictionType' #-}

instance FromText GeoRestrictionType where
    parser = (GeoRestrictionType' . mk) <$> takeText

instance ToText GeoRestrictionType where
    toText (GeoRestrictionType' ci) = original ci

-- | Represents an enum of /known/ $GeoRestrictionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GeoRestrictionType where
    toEnum i = case i of
        0 -> Blacklist
        1 -> None
        2 -> Whitelist
        _ -> (error . showText) $ "Unknown index for GeoRestrictionType: " <> toText i
    fromEnum x = case x of
        Blacklist -> 0
        None -> 1
        Whitelist -> 2
        GeoRestrictionType' name -> (error . showText) $ "Unknown GeoRestrictionType: " <> original name

-- | Represents the bounds of /known/ $GeoRestrictionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GeoRestrictionType where
    minBound = Blacklist
    maxBound = Whitelist

instance Hashable     GeoRestrictionType
instance NFData       GeoRestrictionType
instance ToByteString GeoRestrictionType
instance ToQuery      GeoRestrictionType
instance ToHeader     GeoRestrictionType

instance FromXML GeoRestrictionType where
    parseXML = parseXMLText "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXML = toXMLText
