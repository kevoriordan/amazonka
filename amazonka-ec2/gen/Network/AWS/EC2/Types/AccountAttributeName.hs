{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AccountAttributeName (
  AccountAttributeName (
    ..
    , DefaultVPC
    , SupportedPlatforms
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AccountAttributeName = AccountAttributeName' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern DefaultVPC :: AccountAttributeName
pattern DefaultVPC = AccountAttributeName' "default-vpc"

pattern SupportedPlatforms :: AccountAttributeName
pattern SupportedPlatforms = AccountAttributeName' "supported-platforms"

{-# COMPLETE
  DefaultVPC,
  SupportedPlatforms,
  AccountAttributeName' #-}

instance FromText AccountAttributeName where
    parser = (AccountAttributeName' . mk) <$> takeText

instance ToText AccountAttributeName where
    toText (AccountAttributeName' ci) = original ci

-- | Represents an enum of /known/ $AccountAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccountAttributeName where
    toEnum i = case i of
        0 -> DefaultVPC
        1 -> SupportedPlatforms
        _ -> (error . showText) $ "Unknown index for AccountAttributeName: " <> toText i
    fromEnum x = case x of
        DefaultVPC -> 0
        SupportedPlatforms -> 1
        AccountAttributeName' name -> (error . showText) $ "Unknown AccountAttributeName: " <> original name

-- | Represents the bounds of /known/ $AccountAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccountAttributeName where
    minBound = DefaultVPC
    maxBound = SupportedPlatforms

instance Hashable     AccountAttributeName
instance NFData       AccountAttributeName
instance ToByteString AccountAttributeName
instance ToQuery      AccountAttributeName
instance ToHeader     AccountAttributeName
