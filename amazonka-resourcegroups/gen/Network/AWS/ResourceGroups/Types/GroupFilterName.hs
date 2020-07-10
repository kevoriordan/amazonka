{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.GroupFilterName (
  GroupFilterName (
    ..
    , GFNResourceType
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GroupFilterName = GroupFilterName' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern GFNResourceType :: GroupFilterName
pattern GFNResourceType = GroupFilterName' "resource-type"

{-# COMPLETE
  GFNResourceType,
  GroupFilterName' #-}

instance FromText GroupFilterName where
    parser = (GroupFilterName' . mk) <$> takeText

instance ToText GroupFilterName where
    toText (GroupFilterName' ci) = original ci

-- | Represents an enum of /known/ $GroupFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GroupFilterName where
    toEnum i = case i of
        0 -> GFNResourceType
        _ -> (error . showText) $ "Unknown index for GroupFilterName: " <> toText i
    fromEnum x = case x of
        GFNResourceType -> 0
        GroupFilterName' name -> (error . showText) $ "Unknown GroupFilterName: " <> original name

-- | Represents the bounds of /known/ $GroupFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GroupFilterName where
    minBound = GFNResourceType
    maxBound = GFNResourceType

instance Hashable     GroupFilterName
instance NFData       GroupFilterName
instance ToByteString GroupFilterName
instance ToQuery      GroupFilterName
instance ToHeader     GroupFilterName

instance ToJSON GroupFilterName where
    toJSON = toJSONText
