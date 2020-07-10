{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NodeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.NodeType (
  NodeType (
    ..
    , Crawler
    , Job
    , Trigger
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NodeType = NodeType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Crawler :: NodeType
pattern Crawler = NodeType' "CRAWLER"

pattern Job :: NodeType
pattern Job = NodeType' "JOB"

pattern Trigger :: NodeType
pattern Trigger = NodeType' "TRIGGER"

{-# COMPLETE
  Crawler,
  Job,
  Trigger,
  NodeType' #-}

instance FromText NodeType where
    parser = (NodeType' . mk) <$> takeText

instance ToText NodeType where
    toText (NodeType' ci) = original ci

-- | Represents an enum of /known/ $NodeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NodeType where
    toEnum i = case i of
        0 -> Crawler
        1 -> Job
        2 -> Trigger
        _ -> (error . showText) $ "Unknown index for NodeType: " <> toText i
    fromEnum x = case x of
        Crawler -> 0
        Job -> 1
        Trigger -> 2
        NodeType' name -> (error . showText) $ "Unknown NodeType: " <> original name

-- | Represents the bounds of /known/ $NodeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NodeType where
    minBound = Crawler
    maxBound = Trigger

instance Hashable     NodeType
instance NFData       NodeType
instance ToByteString NodeType
instance ToQuery      NodeType
instance ToHeader     NodeType

instance FromJSON NodeType where
    parseJSON = parseJSONText "NodeType"
