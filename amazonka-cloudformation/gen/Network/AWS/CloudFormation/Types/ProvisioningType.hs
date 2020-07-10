{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ProvisioningType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ProvisioningType (
  ProvisioningType (
    ..
    , FullyMutable
    , Immutable
    , NonProvisionable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisioningType = ProvisioningType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern FullyMutable :: ProvisioningType
pattern FullyMutable = ProvisioningType' "FULLY_MUTABLE"

pattern Immutable :: ProvisioningType
pattern Immutable = ProvisioningType' "IMMUTABLE"

pattern NonProvisionable :: ProvisioningType
pattern NonProvisionable = ProvisioningType' "NON_PROVISIONABLE"

{-# COMPLETE
  FullyMutable,
  Immutable,
  NonProvisionable,
  ProvisioningType' #-}

instance FromText ProvisioningType where
    parser = (ProvisioningType' . mk) <$> takeText

instance ToText ProvisioningType where
    toText (ProvisioningType' ci) = original ci

-- | Represents an enum of /known/ $ProvisioningType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisioningType where
    toEnum i = case i of
        0 -> FullyMutable
        1 -> Immutable
        2 -> NonProvisionable
        _ -> (error . showText) $ "Unknown index for ProvisioningType: " <> toText i
    fromEnum x = case x of
        FullyMutable -> 0
        Immutable -> 1
        NonProvisionable -> 2
        ProvisioningType' name -> (error . showText) $ "Unknown ProvisioningType: " <> original name

-- | Represents the bounds of /known/ $ProvisioningType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisioningType where
    minBound = FullyMutable
    maxBound = NonProvisionable

instance Hashable     ProvisioningType
instance NFData       ProvisioningType
instance ToByteString ProvisioningType
instance ToQuery      ProvisioningType
instance ToHeader     ProvisioningType

instance FromXML ProvisioningType where
    parseXML = parseXMLText "ProvisioningType"
