{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.HandshakeResourceType (
  HandshakeResourceType (
    ..
    , Account
    , Email
    , MasterEmail
    , MasterName
    , Notes
    , Organization
    , OrganizationFeatureSet
    , ParentHandshake
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HandshakeResourceType = HandshakeResourceType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Account :: HandshakeResourceType
pattern Account = HandshakeResourceType' "ACCOUNT"

pattern Email :: HandshakeResourceType
pattern Email = HandshakeResourceType' "EMAIL"

pattern MasterEmail :: HandshakeResourceType
pattern MasterEmail = HandshakeResourceType' "MASTER_EMAIL"

pattern MasterName :: HandshakeResourceType
pattern MasterName = HandshakeResourceType' "MASTER_NAME"

pattern Notes :: HandshakeResourceType
pattern Notes = HandshakeResourceType' "NOTES"

pattern Organization :: HandshakeResourceType
pattern Organization = HandshakeResourceType' "ORGANIZATION"

pattern OrganizationFeatureSet :: HandshakeResourceType
pattern OrganizationFeatureSet = HandshakeResourceType' "ORGANIZATION_FEATURE_SET"

pattern ParentHandshake :: HandshakeResourceType
pattern ParentHandshake = HandshakeResourceType' "PARENT_HANDSHAKE"

{-# COMPLETE
  Account,
  Email,
  MasterEmail,
  MasterName,
  Notes,
  Organization,
  OrganizationFeatureSet,
  ParentHandshake,
  HandshakeResourceType' #-}

instance FromText HandshakeResourceType where
    parser = (HandshakeResourceType' . mk) <$> takeText

instance ToText HandshakeResourceType where
    toText (HandshakeResourceType' ci) = original ci

-- | Represents an enum of /known/ $HandshakeResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HandshakeResourceType where
    toEnum i = case i of
        0 -> Account
        1 -> Email
        2 -> MasterEmail
        3 -> MasterName
        4 -> Notes
        5 -> Organization
        6 -> OrganizationFeatureSet
        7 -> ParentHandshake
        _ -> (error . showText) $ "Unknown index for HandshakeResourceType: " <> toText i
    fromEnum x = case x of
        Account -> 0
        Email -> 1
        MasterEmail -> 2
        MasterName -> 3
        Notes -> 4
        Organization -> 5
        OrganizationFeatureSet -> 6
        ParentHandshake -> 7
        HandshakeResourceType' name -> (error . showText) $ "Unknown HandshakeResourceType: " <> original name

-- | Represents the bounds of /known/ $HandshakeResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HandshakeResourceType where
    minBound = Account
    maxBound = ParentHandshake

instance Hashable     HandshakeResourceType
instance NFData       HandshakeResourceType
instance ToByteString HandshakeResourceType
instance ToQuery      HandshakeResourceType
instance ToHeader     HandshakeResourceType

instance FromJSON HandshakeResourceType where
    parseJSON = parseJSONText "HandshakeResourceType"
