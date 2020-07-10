{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.ServiceFilterName (
  ServiceFilterName (
    ..
    , NamespaceId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceFilterName = ServiceFilterName' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern NamespaceId :: ServiceFilterName
pattern NamespaceId = ServiceFilterName' "NAMESPACE_ID"

{-# COMPLETE
  NamespaceId,
  ServiceFilterName' #-}

instance FromText ServiceFilterName where
    parser = (ServiceFilterName' . mk) <$> takeText

instance ToText ServiceFilterName where
    toText (ServiceFilterName' ci) = original ci

-- | Represents an enum of /known/ $ServiceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceFilterName where
    toEnum i = case i of
        0 -> NamespaceId
        _ -> (error . showText) $ "Unknown index for ServiceFilterName: " <> toText i
    fromEnum x = case x of
        NamespaceId -> 0
        ServiceFilterName' name -> (error . showText) $ "Unknown ServiceFilterName: " <> original name

-- | Represents the bounds of /known/ $ServiceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceFilterName where
    minBound = NamespaceId
    maxBound = NamespaceId

instance Hashable     ServiceFilterName
instance NFData       ServiceFilterName
instance ToByteString ServiceFilterName
instance ToQuery      ServiceFilterName
instance ToHeader     ServiceFilterName

instance ToJSON ServiceFilterName where
    toJSON = toJSONText
