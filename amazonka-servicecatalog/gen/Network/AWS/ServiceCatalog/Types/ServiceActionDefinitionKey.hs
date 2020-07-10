{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey (
  ServiceActionDefinitionKey (
    ..
    , AssumeRole
    , Name
    , Parameters
    , Version
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceActionDefinitionKey = ServiceActionDefinitionKey' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern AssumeRole :: ServiceActionDefinitionKey
pattern AssumeRole = ServiceActionDefinitionKey' "AssumeRole"

pattern Name :: ServiceActionDefinitionKey
pattern Name = ServiceActionDefinitionKey' "Name"

pattern Parameters :: ServiceActionDefinitionKey
pattern Parameters = ServiceActionDefinitionKey' "Parameters"

pattern Version :: ServiceActionDefinitionKey
pattern Version = ServiceActionDefinitionKey' "Version"

{-# COMPLETE
  AssumeRole,
  Name,
  Parameters,
  Version,
  ServiceActionDefinitionKey' #-}

instance FromText ServiceActionDefinitionKey where
    parser = (ServiceActionDefinitionKey' . mk) <$> takeText

instance ToText ServiceActionDefinitionKey where
    toText (ServiceActionDefinitionKey' ci) = original ci

-- | Represents an enum of /known/ $ServiceActionDefinitionKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceActionDefinitionKey where
    toEnum i = case i of
        0 -> AssumeRole
        1 -> Name
        2 -> Parameters
        3 -> Version
        _ -> (error . showText) $ "Unknown index for ServiceActionDefinitionKey: " <> toText i
    fromEnum x = case x of
        AssumeRole -> 0
        Name -> 1
        Parameters -> 2
        Version -> 3
        ServiceActionDefinitionKey' name -> (error . showText) $ "Unknown ServiceActionDefinitionKey: " <> original name

-- | Represents the bounds of /known/ $ServiceActionDefinitionKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceActionDefinitionKey where
    minBound = AssumeRole
    maxBound = Version

instance Hashable     ServiceActionDefinitionKey
instance NFData       ServiceActionDefinitionKey
instance ToByteString ServiceActionDefinitionKey
instance ToQuery      ServiceActionDefinitionKey
instance ToHeader     ServiceActionDefinitionKey

instance ToJSON ServiceActionDefinitionKey where
    toJSON = toJSONText

instance FromJSON ServiceActionDefinitionKey where
    parseJSON = parseJSONText "ServiceActionDefinitionKey"
