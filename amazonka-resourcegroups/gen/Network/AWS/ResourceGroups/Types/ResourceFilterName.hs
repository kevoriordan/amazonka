{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.ResourceFilterName (
  ResourceFilterName (
    ..
    , ResourceType
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceFilterName = ResourceFilterName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ResourceType :: ResourceFilterName
pattern ResourceType = ResourceFilterName' "resource-type"

{-# COMPLETE
  ResourceType,
  ResourceFilterName' #-}

instance FromText ResourceFilterName where
    parser = (ResourceFilterName' . mk) <$> takeText

instance ToText ResourceFilterName where
    toText (ResourceFilterName' ci) = original ci

-- | Represents an enum of /known/ $ResourceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceFilterName where
    toEnum i = case i of
        0 -> ResourceType
        _ -> (error . showText) $ "Unknown index for ResourceFilterName: " <> toText i
    fromEnum x = case x of
        ResourceType -> 0
        ResourceFilterName' name -> (error . showText) $ "Unknown ResourceFilterName: " <> original name

-- | Represents the bounds of /known/ $ResourceFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceFilterName where
    minBound = ResourceType
    maxBound = ResourceType

instance Hashable     ResourceFilterName
instance NFData       ResourceFilterName
instance ToByteString ResourceFilterName
instance ToQuery      ResourceFilterName
instance ToHeader     ResourceFilterName

instance ToJSON ResourceFilterName where
    toJSON = toJSONText
