{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ChildType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.ChildType (
  ChildType (
    ..
    , CTAccount
    , CTOrganizationalUnit
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChildType = ChildType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern CTAccount :: ChildType
pattern CTAccount = ChildType' "ACCOUNT"

pattern CTOrganizationalUnit :: ChildType
pattern CTOrganizationalUnit = ChildType' "ORGANIZATIONAL_UNIT"

{-# COMPLETE
  CTAccount,
  CTOrganizationalUnit,
  ChildType' #-}

instance FromText ChildType where
    parser = (ChildType' . mk) <$> takeText

instance ToText ChildType where
    toText (ChildType' ci) = original ci

-- | Represents an enum of /known/ $ChildType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChildType where
    toEnum i = case i of
        0 -> CTAccount
        1 -> CTOrganizationalUnit
        _ -> (error . showText) $ "Unknown index for ChildType: " <> toText i
    fromEnum x = case x of
        CTAccount -> 0
        CTOrganizationalUnit -> 1
        ChildType' name -> (error . showText) $ "Unknown ChildType: " <> original name

-- | Represents the bounds of /known/ $ChildType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChildType where
    minBound = CTAccount
    maxBound = CTOrganizationalUnit

instance Hashable     ChildType
instance NFData       ChildType
instance ToByteString ChildType
instance ToQuery      ChildType
instance ToHeader     ChildType

instance ToJSON ChildType where
    toJSON = toJSONText

instance FromJSON ChildType where
    parseJSON = parseJSONText "ChildType"
