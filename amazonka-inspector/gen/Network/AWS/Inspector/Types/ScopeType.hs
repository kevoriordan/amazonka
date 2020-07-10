{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ScopeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.ScopeType (
  ScopeType (
    ..
    , InstanceId
    , RulesPackageARN
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScopeType = ScopeType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern InstanceId :: ScopeType
pattern InstanceId = ScopeType' "INSTANCE_ID"

pattern RulesPackageARN :: ScopeType
pattern RulesPackageARN = ScopeType' "RULES_PACKAGE_ARN"

{-# COMPLETE
  InstanceId,
  RulesPackageARN,
  ScopeType' #-}

instance FromText ScopeType where
    parser = (ScopeType' . mk) <$> takeText

instance ToText ScopeType where
    toText (ScopeType' ci) = original ci

-- | Represents an enum of /known/ $ScopeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScopeType where
    toEnum i = case i of
        0 -> InstanceId
        1 -> RulesPackageARN
        _ -> (error . showText) $ "Unknown index for ScopeType: " <> toText i
    fromEnum x = case x of
        InstanceId -> 0
        RulesPackageARN -> 1
        ScopeType' name -> (error . showText) $ "Unknown ScopeType: " <> original name

-- | Represents the bounds of /known/ $ScopeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScopeType where
    minBound = InstanceId
    maxBound = RulesPackageARN

instance Hashable     ScopeType
instance NFData       ScopeType
instance ToByteString ScopeType
instance ToQuery      ScopeType
instance ToHeader     ScopeType

instance FromJSON ScopeType where
    parseJSON = parseJSONText "ScopeType"
