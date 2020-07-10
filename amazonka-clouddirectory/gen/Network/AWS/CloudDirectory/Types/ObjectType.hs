{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.ObjectType (
  ObjectType (
    ..
    , Index
    , LeafNode
    , Node
    , Policy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ObjectType = ObjectType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Index :: ObjectType
pattern Index = ObjectType' "INDEX"

pattern LeafNode :: ObjectType
pattern LeafNode = ObjectType' "LEAF_NODE"

pattern Node :: ObjectType
pattern Node = ObjectType' "NODE"

pattern Policy :: ObjectType
pattern Policy = ObjectType' "POLICY"

{-# COMPLETE
  Index,
  LeafNode,
  Node,
  Policy,
  ObjectType' #-}

instance FromText ObjectType where
    parser = (ObjectType' . mk) <$> takeText

instance ToText ObjectType where
    toText (ObjectType' ci) = original ci

-- | Represents an enum of /known/ $ObjectType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectType where
    toEnum i = case i of
        0 -> Index
        1 -> LeafNode
        2 -> Node
        3 -> Policy
        _ -> (error . showText) $ "Unknown index for ObjectType: " <> toText i
    fromEnum x = case x of
        Index -> 0
        LeafNode -> 1
        Node -> 2
        Policy -> 3
        ObjectType' name -> (error . showText) $ "Unknown ObjectType: " <> original name

-- | Represents the bounds of /known/ $ObjectType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectType where
    minBound = Index
    maxBound = Policy

instance Hashable     ObjectType
instance NFData       ObjectType
instance ToByteString ObjectType
instance ToQuery      ObjectType
instance ToHeader     ObjectType

instance ToJSON ObjectType where
    toJSON = toJSONText

instance FromJSON ObjectType where
    parseJSON = parseJSONText "ObjectType"
