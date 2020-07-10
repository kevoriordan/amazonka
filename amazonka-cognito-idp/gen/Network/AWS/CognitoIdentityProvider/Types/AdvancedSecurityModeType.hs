{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType (
  AdvancedSecurityModeType (
    ..
    , Audit
    , Enforced
    , Off
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AdvancedSecurityModeType = AdvancedSecurityModeType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Audit :: AdvancedSecurityModeType
pattern Audit = AdvancedSecurityModeType' "AUDIT"

pattern Enforced :: AdvancedSecurityModeType
pattern Enforced = AdvancedSecurityModeType' "ENFORCED"

pattern Off :: AdvancedSecurityModeType
pattern Off = AdvancedSecurityModeType' "OFF"

{-# COMPLETE
  Audit,
  Enforced,
  Off,
  AdvancedSecurityModeType' #-}

instance FromText AdvancedSecurityModeType where
    parser = (AdvancedSecurityModeType' . mk) <$> takeText

instance ToText AdvancedSecurityModeType where
    toText (AdvancedSecurityModeType' ci) = original ci

-- | Represents an enum of /known/ $AdvancedSecurityModeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdvancedSecurityModeType where
    toEnum i = case i of
        0 -> Audit
        1 -> Enforced
        2 -> Off
        _ -> (error . showText) $ "Unknown index for AdvancedSecurityModeType: " <> toText i
    fromEnum x = case x of
        Audit -> 0
        Enforced -> 1
        Off -> 2
        AdvancedSecurityModeType' name -> (error . showText) $ "Unknown AdvancedSecurityModeType: " <> original name

-- | Represents the bounds of /known/ $AdvancedSecurityModeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdvancedSecurityModeType where
    minBound = Audit
    maxBound = Off

instance Hashable     AdvancedSecurityModeType
instance NFData       AdvancedSecurityModeType
instance ToByteString AdvancedSecurityModeType
instance ToQuery      AdvancedSecurityModeType
instance ToHeader     AdvancedSecurityModeType

instance ToJSON AdvancedSecurityModeType where
    toJSON = toJSONText

instance FromJSON AdvancedSecurityModeType where
    parseJSON = parseJSONText "AdvancedSecurityModeType"
