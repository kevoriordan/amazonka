{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType (
  ServiceActionDefinitionType (
    ..
    , SsmAutomation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceActionDefinitionType = ServiceActionDefinitionType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern SsmAutomation :: ServiceActionDefinitionType
pattern SsmAutomation = ServiceActionDefinitionType' "SSM_AUTOMATION"

{-# COMPLETE
  SsmAutomation,
  ServiceActionDefinitionType' #-}

instance FromText ServiceActionDefinitionType where
    parser = (ServiceActionDefinitionType' . mk) <$> takeText

instance ToText ServiceActionDefinitionType where
    toText (ServiceActionDefinitionType' ci) = original ci

-- | Represents an enum of /known/ $ServiceActionDefinitionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceActionDefinitionType where
    toEnum i = case i of
        0 -> SsmAutomation
        _ -> (error . showText) $ "Unknown index for ServiceActionDefinitionType: " <> toText i
    fromEnum x = case x of
        SsmAutomation -> 0
        ServiceActionDefinitionType' name -> (error . showText) $ "Unknown ServiceActionDefinitionType: " <> original name

-- | Represents the bounds of /known/ $ServiceActionDefinitionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceActionDefinitionType where
    minBound = SsmAutomation
    maxBound = SsmAutomation

instance Hashable     ServiceActionDefinitionType
instance NFData       ServiceActionDefinitionType
instance ToByteString ServiceActionDefinitionType
instance ToQuery      ServiceActionDefinitionType
instance ToHeader     ServiceActionDefinitionType

instance ToJSON ServiceActionDefinitionType where
    toJSON = toJSONText

instance FromJSON ServiceActionDefinitionType where
    parseJSON = parseJSONText "ServiceActionDefinitionType"
