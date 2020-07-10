{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.EffectivePolicyType (
  EffectivePolicyType (
    ..
    , EPTTagPolicy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EffectivePolicyType = EffectivePolicyType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern EPTTagPolicy :: EffectivePolicyType
pattern EPTTagPolicy = EffectivePolicyType' "TAG_POLICY"

{-# COMPLETE
  EPTTagPolicy,
  EffectivePolicyType' #-}

instance FromText EffectivePolicyType where
    parser = (EffectivePolicyType' . mk) <$> takeText

instance ToText EffectivePolicyType where
    toText (EffectivePolicyType' ci) = original ci

-- | Represents an enum of /known/ $EffectivePolicyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EffectivePolicyType where
    toEnum i = case i of
        0 -> EPTTagPolicy
        _ -> (error . showText) $ "Unknown index for EffectivePolicyType: " <> toText i
    fromEnum x = case x of
        EPTTagPolicy -> 0
        EffectivePolicyType' name -> (error . showText) $ "Unknown EffectivePolicyType: " <> original name

-- | Represents the bounds of /known/ $EffectivePolicyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EffectivePolicyType where
    minBound = EPTTagPolicy
    maxBound = EPTTagPolicy

instance Hashable     EffectivePolicyType
instance NFData       EffectivePolicyType
instance ToByteString EffectivePolicyType
instance ToQuery      EffectivePolicyType
instance ToHeader     EffectivePolicyType

instance ToJSON EffectivePolicyType where
    toJSON = toJSONText

instance FromJSON EffectivePolicyType where
    parseJSON = parseJSONText "EffectivePolicyType"
