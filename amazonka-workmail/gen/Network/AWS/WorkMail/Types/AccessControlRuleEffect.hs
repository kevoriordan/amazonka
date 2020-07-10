{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.AccessControlRuleEffect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.AccessControlRuleEffect (
  AccessControlRuleEffect (
    ..
    , Allow
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccessControlRuleEffect = AccessControlRuleEffect' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Allow :: AccessControlRuleEffect
pattern Allow = AccessControlRuleEffect' "ALLOW"

pattern Deny :: AccessControlRuleEffect
pattern Deny = AccessControlRuleEffect' "DENY"

{-# COMPLETE
  Allow,
  Deny,
  AccessControlRuleEffect' #-}

instance FromText AccessControlRuleEffect where
    parser = (AccessControlRuleEffect' . mk) <$> takeText

instance ToText AccessControlRuleEffect where
    toText (AccessControlRuleEffect' ci) = original ci

-- | Represents an enum of /known/ $AccessControlRuleEffect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccessControlRuleEffect where
    toEnum i = case i of
        0 -> Allow
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for AccessControlRuleEffect: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Deny -> 1
        AccessControlRuleEffect' name -> (error . showText) $ "Unknown AccessControlRuleEffect: " <> original name

-- | Represents the bounds of /known/ $AccessControlRuleEffect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccessControlRuleEffect where
    minBound = Allow
    maxBound = Deny

instance Hashable     AccessControlRuleEffect
instance NFData       AccessControlRuleEffect
instance ToByteString AccessControlRuleEffect
instance ToQuery      AccessControlRuleEffect
instance ToHeader     AccessControlRuleEffect

instance ToJSON AccessControlRuleEffect where
    toJSON = toJSONText

instance FromJSON AccessControlRuleEffect where
    parseJSON = parseJSONText "AccessControlRuleEffect"
