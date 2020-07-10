{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute (
  GroupByAttribute (
    ..
    , Region
    , ResourceType
    , TargetId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GroupByAttribute = GroupByAttribute' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Region :: GroupByAttribute
pattern Region = GroupByAttribute' "REGION"

pattern ResourceType :: GroupByAttribute
pattern ResourceType = GroupByAttribute' "RESOURCE_TYPE"

pattern TargetId :: GroupByAttribute
pattern TargetId = GroupByAttribute' "TARGET_ID"

{-# COMPLETE
  Region,
  ResourceType,
  TargetId,
  GroupByAttribute' #-}

instance FromText GroupByAttribute where
    parser = (GroupByAttribute' . mk) <$> takeText

instance ToText GroupByAttribute where
    toText (GroupByAttribute' ci) = original ci

-- | Represents an enum of /known/ $GroupByAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GroupByAttribute where
    toEnum i = case i of
        0 -> Region
        1 -> ResourceType
        2 -> TargetId
        _ -> (error . showText) $ "Unknown index for GroupByAttribute: " <> toText i
    fromEnum x = case x of
        Region -> 0
        ResourceType -> 1
        TargetId -> 2
        GroupByAttribute' name -> (error . showText) $ "Unknown GroupByAttribute: " <> original name

-- | Represents the bounds of /known/ $GroupByAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GroupByAttribute where
    minBound = Region
    maxBound = TargetId

instance Hashable     GroupByAttribute
instance NFData       GroupByAttribute
instance ToByteString GroupByAttribute
instance ToQuery      GroupByAttribute
instance ToHeader     GroupByAttribute

instance ToJSON GroupByAttribute where
    toJSON = toJSONText
