{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TargetIdType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.TargetIdType (
  TargetIdType (
    ..
    , Account
    , OU
    , Root
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetIdType = TargetIdType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Account :: TargetIdType
pattern Account = TargetIdType' "ACCOUNT"

pattern OU :: TargetIdType
pattern OU = TargetIdType' "OU"

pattern Root :: TargetIdType
pattern Root = TargetIdType' "ROOT"

{-# COMPLETE
  Account,
  OU,
  Root,
  TargetIdType' #-}

instance FromText TargetIdType where
    parser = (TargetIdType' . mk) <$> takeText

instance ToText TargetIdType where
    toText (TargetIdType' ci) = original ci

-- | Represents an enum of /known/ $TargetIdType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetIdType where
    toEnum i = case i of
        0 -> Account
        1 -> OU
        2 -> Root
        _ -> (error . showText) $ "Unknown index for TargetIdType: " <> toText i
    fromEnum x = case x of
        Account -> 0
        OU -> 1
        Root -> 2
        TargetIdType' name -> (error . showText) $ "Unknown TargetIdType: " <> original name

-- | Represents the bounds of /known/ $TargetIdType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetIdType where
    minBound = Account
    maxBound = Root

instance Hashable     TargetIdType
instance NFData       TargetIdType
instance ToByteString TargetIdType
instance ToQuery      TargetIdType
instance ToHeader     TargetIdType

instance FromJSON TargetIdType where
    parseJSON = parseJSONText "TargetIdType"
