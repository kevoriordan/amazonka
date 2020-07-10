{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyScopeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyScopeType (
  PolicyScopeType (
    ..
    , AWS
    , All
    , Local
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyScopeType = PolicyScopeType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AWS :: PolicyScopeType
pattern AWS = PolicyScopeType' "AWS"

pattern All :: PolicyScopeType
pattern All = PolicyScopeType' "All"

pattern Local :: PolicyScopeType
pattern Local = PolicyScopeType' "Local"

{-# COMPLETE
  AWS,
  All,
  Local,
  PolicyScopeType' #-}

instance FromText PolicyScopeType where
    parser = (PolicyScopeType' . mk) <$> takeText

instance ToText PolicyScopeType where
    toText (PolicyScopeType' ci) = original ci

-- | Represents an enum of /known/ $PolicyScopeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyScopeType where
    toEnum i = case i of
        0 -> AWS
        1 -> All
        2 -> Local
        _ -> (error . showText) $ "Unknown index for PolicyScopeType: " <> toText i
    fromEnum x = case x of
        AWS -> 0
        All -> 1
        Local -> 2
        PolicyScopeType' name -> (error . showText) $ "Unknown PolicyScopeType: " <> original name

-- | Represents the bounds of /known/ $PolicyScopeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyScopeType where
    minBound = AWS
    maxBound = Local

instance Hashable     PolicyScopeType
instance NFData       PolicyScopeType
instance ToByteString PolicyScopeType
instance ToQuery      PolicyScopeType
instance ToHeader     PolicyScopeType
