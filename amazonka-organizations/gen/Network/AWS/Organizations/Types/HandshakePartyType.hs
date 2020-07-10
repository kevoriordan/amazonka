{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakePartyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.HandshakePartyType (
  HandshakePartyType (
    ..
    , HPTAccount
    , HPTEmail
    , HPTOrganization
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HandshakePartyType = HandshakePartyType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern HPTAccount :: HandshakePartyType
pattern HPTAccount = HandshakePartyType' "ACCOUNT"

pattern HPTEmail :: HandshakePartyType
pattern HPTEmail = HandshakePartyType' "EMAIL"

pattern HPTOrganization :: HandshakePartyType
pattern HPTOrganization = HandshakePartyType' "ORGANIZATION"

{-# COMPLETE
  HPTAccount,
  HPTEmail,
  HPTOrganization,
  HandshakePartyType' #-}

instance FromText HandshakePartyType where
    parser = (HandshakePartyType' . mk) <$> takeText

instance ToText HandshakePartyType where
    toText (HandshakePartyType' ci) = original ci

-- | Represents an enum of /known/ $HandshakePartyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HandshakePartyType where
    toEnum i = case i of
        0 -> HPTAccount
        1 -> HPTEmail
        2 -> HPTOrganization
        _ -> (error . showText) $ "Unknown index for HandshakePartyType: " <> toText i
    fromEnum x = case x of
        HPTAccount -> 0
        HPTEmail -> 1
        HPTOrganization -> 2
        HandshakePartyType' name -> (error . showText) $ "Unknown HandshakePartyType: " <> original name

-- | Represents the bounds of /known/ $HandshakePartyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HandshakePartyType where
    minBound = HPTAccount
    maxBound = HPTOrganization

instance Hashable     HandshakePartyType
instance NFData       HandshakePartyType
instance ToByteString HandshakePartyType
instance ToQuery      HandshakePartyType
instance ToHeader     HandshakePartyType

instance ToJSON HandshakePartyType where
    toJSON = toJSONText

instance FromJSON HandshakePartyType where
    parseJSON = parseJSONText "HandshakePartyType"
