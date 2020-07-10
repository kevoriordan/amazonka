{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DomainType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DomainType (
  DomainType (
    ..
    , DTStandard
    , DTVPC
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DomainType = DomainType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern DTStandard :: DomainType
pattern DTStandard = DomainType' "standard"

pattern DTVPC :: DomainType
pattern DTVPC = DomainType' "vpc"

{-# COMPLETE
  DTStandard,
  DTVPC,
  DomainType' #-}

instance FromText DomainType where
    parser = (DomainType' . mk) <$> takeText

instance ToText DomainType where
    toText (DomainType' ci) = original ci

-- | Represents an enum of /known/ $DomainType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainType where
    toEnum i = case i of
        0 -> DTStandard
        1 -> DTVPC
        _ -> (error . showText) $ "Unknown index for DomainType: " <> toText i
    fromEnum x = case x of
        DTStandard -> 0
        DTVPC -> 1
        DomainType' name -> (error . showText) $ "Unknown DomainType: " <> original name

-- | Represents the bounds of /known/ $DomainType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainType where
    minBound = DTStandard
    maxBound = DTVPC

instance Hashable     DomainType
instance NFData       DomainType
instance ToByteString DomainType
instance ToQuery      DomainType
instance ToHeader     DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"
