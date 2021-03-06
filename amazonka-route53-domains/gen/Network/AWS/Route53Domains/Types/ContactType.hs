{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.ContactType (
  ContactType (
    ..
    , Association
    , Company
    , Person
    , PublicBody
    , Reseller
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContactType = ContactType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Association :: ContactType
pattern Association = ContactType' "ASSOCIATION"

pattern Company :: ContactType
pattern Company = ContactType' "COMPANY"

pattern Person :: ContactType
pattern Person = ContactType' "PERSON"

pattern PublicBody :: ContactType
pattern PublicBody = ContactType' "PUBLIC_BODY"

pattern Reseller :: ContactType
pattern Reseller = ContactType' "RESELLER"

{-# COMPLETE
  Association,
  Company,
  Person,
  PublicBody,
  Reseller,
  ContactType' #-}

instance FromText ContactType where
    parser = (ContactType' . mk) <$> takeText

instance ToText ContactType where
    toText (ContactType' ci) = original ci

-- | Represents an enum of /known/ $ContactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContactType where
    toEnum i = case i of
        0 -> Association
        1 -> Company
        2 -> Person
        3 -> PublicBody
        4 -> Reseller
        _ -> (error . showText) $ "Unknown index for ContactType: " <> toText i
    fromEnum x = case x of
        Association -> 0
        Company -> 1
        Person -> 2
        PublicBody -> 3
        Reseller -> 4
        ContactType' name -> (error . showText) $ "Unknown ContactType: " <> original name

-- | Represents the bounds of /known/ $ContactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContactType where
    minBound = Association
    maxBound = Reseller

instance Hashable     ContactType
instance NFData       ContactType
instance ToByteString ContactType
instance ToQuery      ContactType
instance ToHeader     ContactType

instance ToJSON ContactType where
    toJSON = toJSONText

instance FromJSON ContactType where
    parseJSON = parseJSONText "ContactType"
