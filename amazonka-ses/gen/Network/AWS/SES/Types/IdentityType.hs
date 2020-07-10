{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.IdentityType (
  IdentityType (
    ..
    , Domain
    , EmailAddress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IdentityType = IdentityType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Domain :: IdentityType
pattern Domain = IdentityType' "Domain"

pattern EmailAddress :: IdentityType
pattern EmailAddress = IdentityType' "EmailAddress"

{-# COMPLETE
  Domain,
  EmailAddress,
  IdentityType' #-}

instance FromText IdentityType where
    parser = (IdentityType' . mk) <$> takeText

instance ToText IdentityType where
    toText (IdentityType' ci) = original ci

-- | Represents an enum of /known/ $IdentityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IdentityType where
    toEnum i = case i of
        0 -> Domain
        1 -> EmailAddress
        _ -> (error . showText) $ "Unknown index for IdentityType: " <> toText i
    fromEnum x = case x of
        Domain -> 0
        EmailAddress -> 1
        IdentityType' name -> (error . showText) $ "Unknown IdentityType: " <> original name

-- | Represents the bounds of /known/ $IdentityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IdentityType where
    minBound = Domain
    maxBound = EmailAddress

instance Hashable     IdentityType
instance NFData       IdentityType
instance ToByteString IdentityType
instance ToQuery      IdentityType
instance ToHeader     IdentityType
