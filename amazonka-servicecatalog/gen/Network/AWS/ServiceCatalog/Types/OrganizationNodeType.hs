{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNodeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.OrganizationNodeType (
  OrganizationNodeType (
    ..
    , ONTAccount
    , ONTOrganization
    , ONTOrganizationalUnit
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrganizationNodeType = OrganizationNodeType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ONTAccount :: OrganizationNodeType
pattern ONTAccount = OrganizationNodeType' "ACCOUNT"

pattern ONTOrganization :: OrganizationNodeType
pattern ONTOrganization = OrganizationNodeType' "ORGANIZATION"

pattern ONTOrganizationalUnit :: OrganizationNodeType
pattern ONTOrganizationalUnit = OrganizationNodeType' "ORGANIZATIONAL_UNIT"

{-# COMPLETE
  ONTAccount,
  ONTOrganization,
  ONTOrganizationalUnit,
  OrganizationNodeType' #-}

instance FromText OrganizationNodeType where
    parser = (OrganizationNodeType' . mk) <$> takeText

instance ToText OrganizationNodeType where
    toText (OrganizationNodeType' ci) = original ci

-- | Represents an enum of /known/ $OrganizationNodeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrganizationNodeType where
    toEnum i = case i of
        0 -> ONTAccount
        1 -> ONTOrganization
        2 -> ONTOrganizationalUnit
        _ -> (error . showText) $ "Unknown index for OrganizationNodeType: " <> toText i
    fromEnum x = case x of
        ONTAccount -> 0
        ONTOrganization -> 1
        ONTOrganizationalUnit -> 2
        OrganizationNodeType' name -> (error . showText) $ "Unknown OrganizationNodeType: " <> original name

-- | Represents the bounds of /known/ $OrganizationNodeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrganizationNodeType where
    minBound = ONTAccount
    maxBound = ONTOrganizationalUnit

instance Hashable     OrganizationNodeType
instance NFData       OrganizationNodeType
instance ToByteString OrganizationNodeType
instance ToQuery      OrganizationNodeType
instance ToHeader     OrganizationNodeType

instance ToJSON OrganizationNodeType where
    toJSON = toJSONText

instance FromJSON OrganizationNodeType where
    parseJSON = parseJSONText "OrganizationNodeType"
