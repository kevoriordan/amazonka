{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupType (
  InstanceGroupType (
    ..
    , Core
    , Master
    , Task
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceGroupType = InstanceGroupType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Core :: InstanceGroupType
pattern Core = InstanceGroupType' "CORE"

pattern Master :: InstanceGroupType
pattern Master = InstanceGroupType' "MASTER"

pattern Task :: InstanceGroupType
pattern Task = InstanceGroupType' "TASK"

{-# COMPLETE
  Core,
  Master,
  Task,
  InstanceGroupType' #-}

instance FromText InstanceGroupType where
    parser = (InstanceGroupType' . mk) <$> takeText

instance ToText InstanceGroupType where
    toText (InstanceGroupType' ci) = original ci

-- | Represents an enum of /known/ $InstanceGroupType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceGroupType where
    toEnum i = case i of
        0 -> Core
        1 -> Master
        2 -> Task
        _ -> (error . showText) $ "Unknown index for InstanceGroupType: " <> toText i
    fromEnum x = case x of
        Core -> 0
        Master -> 1
        Task -> 2
        InstanceGroupType' name -> (error . showText) $ "Unknown InstanceGroupType: " <> original name

-- | Represents the bounds of /known/ $InstanceGroupType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceGroupType where
    minBound = Core
    maxBound = Task

instance Hashable     InstanceGroupType
instance NFData       InstanceGroupType
instance ToByteString InstanceGroupType
instance ToQuery      InstanceGroupType
instance ToHeader     InstanceGroupType

instance ToJSON InstanceGroupType where
    toJSON = toJSONText

instance FromJSON InstanceGroupType where
    parseJSON = parseJSONText "InstanceGroupType"
