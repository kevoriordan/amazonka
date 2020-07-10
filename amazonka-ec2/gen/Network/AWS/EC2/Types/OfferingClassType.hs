{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OfferingClassType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.OfferingClassType (
  OfferingClassType (
    ..
    , OCTConvertible
    , OCTStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OfferingClassType = OfferingClassType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern OCTConvertible :: OfferingClassType
pattern OCTConvertible = OfferingClassType' "convertible"

pattern OCTStandard :: OfferingClassType
pattern OCTStandard = OfferingClassType' "standard"

{-# COMPLETE
  OCTConvertible,
  OCTStandard,
  OfferingClassType' #-}

instance FromText OfferingClassType where
    parser = (OfferingClassType' . mk) <$> takeText

instance ToText OfferingClassType where
    toText (OfferingClassType' ci) = original ci

-- | Represents an enum of /known/ $OfferingClassType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingClassType where
    toEnum i = case i of
        0 -> OCTConvertible
        1 -> OCTStandard
        _ -> (error . showText) $ "Unknown index for OfferingClassType: " <> toText i
    fromEnum x = case x of
        OCTConvertible -> 0
        OCTStandard -> 1
        OfferingClassType' name -> (error . showText) $ "Unknown OfferingClassType: " <> original name

-- | Represents the bounds of /known/ $OfferingClassType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingClassType where
    minBound = OCTConvertible
    maxBound = OCTStandard

instance Hashable     OfferingClassType
instance NFData       OfferingClassType
instance ToByteString OfferingClassType
instance ToQuery      OfferingClassType
instance ToHeader     OfferingClassType

instance FromXML OfferingClassType where
    parseXML = parseXMLText "OfferingClassType"
