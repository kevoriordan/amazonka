{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RegistryType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.RegistryType (
  RegistryType (
    ..
    , Resource
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RegistryType = RegistryType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Resource :: RegistryType
pattern Resource = RegistryType' "RESOURCE"

{-# COMPLETE
  Resource,
  RegistryType' #-}

instance FromText RegistryType where
    parser = (RegistryType' . mk) <$> takeText

instance ToText RegistryType where
    toText (RegistryType' ci) = original ci

-- | Represents an enum of /known/ $RegistryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RegistryType where
    toEnum i = case i of
        0 -> Resource
        _ -> (error . showText) $ "Unknown index for RegistryType: " <> toText i
    fromEnum x = case x of
        Resource -> 0
        RegistryType' name -> (error . showText) $ "Unknown RegistryType: " <> original name

-- | Represents the bounds of /known/ $RegistryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RegistryType where
    minBound = Resource
    maxBound = Resource

instance Hashable     RegistryType
instance NFData       RegistryType
instance ToByteString RegistryType
instance ToQuery      RegistryType
instance ToHeader     RegistryType

instance FromXML RegistryType where
    parseXML = parseXMLText "RegistryType"
