{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyOwnerEntityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyOwnerEntityType (
  PolicyOwnerEntityType (
    ..
    , POETGroup
    , POETRole
    , POETUser
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyOwnerEntityType = PolicyOwnerEntityType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern POETGroup :: PolicyOwnerEntityType
pattern POETGroup = PolicyOwnerEntityType' "GROUP"

pattern POETRole :: PolicyOwnerEntityType
pattern POETRole = PolicyOwnerEntityType' "ROLE"

pattern POETUser :: PolicyOwnerEntityType
pattern POETUser = PolicyOwnerEntityType' "USER"

{-# COMPLETE
  POETGroup,
  POETRole,
  POETUser,
  PolicyOwnerEntityType' #-}

instance FromText PolicyOwnerEntityType where
    parser = (PolicyOwnerEntityType' . mk) <$> takeText

instance ToText PolicyOwnerEntityType where
    toText (PolicyOwnerEntityType' ci) = original ci

-- | Represents an enum of /known/ $PolicyOwnerEntityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyOwnerEntityType where
    toEnum i = case i of
        0 -> POETGroup
        1 -> POETRole
        2 -> POETUser
        _ -> (error . showText) $ "Unknown index for PolicyOwnerEntityType: " <> toText i
    fromEnum x = case x of
        POETGroup -> 0
        POETRole -> 1
        POETUser -> 2
        PolicyOwnerEntityType' name -> (error . showText) $ "Unknown PolicyOwnerEntityType: " <> original name

-- | Represents the bounds of /known/ $PolicyOwnerEntityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyOwnerEntityType where
    minBound = POETGroup
    maxBound = POETUser

instance Hashable     PolicyOwnerEntityType
instance NFData       PolicyOwnerEntityType
instance ToByteString PolicyOwnerEntityType
instance ToQuery      PolicyOwnerEntityType
instance ToHeader     PolicyOwnerEntityType

instance FromXML PolicyOwnerEntityType where
    parseXML = parseXMLText "PolicyOwnerEntityType"
