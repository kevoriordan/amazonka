{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.NamespaceFilterName (
  NamespaceFilterName (
    ..
    , Type
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NamespaceFilterName = NamespaceFilterName' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Type :: NamespaceFilterName
pattern Type = NamespaceFilterName' "TYPE"

{-# COMPLETE
  Type,
  NamespaceFilterName' #-}

instance FromText NamespaceFilterName where
    parser = (NamespaceFilterName' . mk) <$> takeText

instance ToText NamespaceFilterName where
    toText (NamespaceFilterName' ci) = original ci

-- | Represents an enum of /known/ $NamespaceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NamespaceFilterName where
    toEnum i = case i of
        0 -> Type
        _ -> (error . showText) $ "Unknown index for NamespaceFilterName: " <> toText i
    fromEnum x = case x of
        Type -> 0
        NamespaceFilterName' name -> (error . showText) $ "Unknown NamespaceFilterName: " <> original name

-- | Represents the bounds of /known/ $NamespaceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NamespaceFilterName where
    minBound = Type
    maxBound = Type

instance Hashable     NamespaceFilterName
instance NFData       NamespaceFilterName
instance ToByteString NamespaceFilterName
instance ToQuery      NamespaceFilterName
instance ToHeader     NamespaceFilterName

instance ToJSON NamespaceFilterName where
    toJSON = toJSONText
