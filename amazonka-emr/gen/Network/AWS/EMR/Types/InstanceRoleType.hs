{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceRoleType (
  InstanceRoleType (
    ..
    , IRTCore
    , IRTMaster
    , IRTTask
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceRoleType = InstanceRoleType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern IRTCore :: InstanceRoleType
pattern IRTCore = InstanceRoleType' "CORE"

pattern IRTMaster :: InstanceRoleType
pattern IRTMaster = InstanceRoleType' "MASTER"

pattern IRTTask :: InstanceRoleType
pattern IRTTask = InstanceRoleType' "TASK"

{-# COMPLETE
  IRTCore,
  IRTMaster,
  IRTTask,
  InstanceRoleType' #-}

instance FromText InstanceRoleType where
    parser = (InstanceRoleType' . mk) <$> takeText

instance ToText InstanceRoleType where
    toText (InstanceRoleType' ci) = original ci

-- | Represents an enum of /known/ $InstanceRoleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceRoleType where
    toEnum i = case i of
        0 -> IRTCore
        1 -> IRTMaster
        2 -> IRTTask
        _ -> (error . showText) $ "Unknown index for InstanceRoleType: " <> toText i
    fromEnum x = case x of
        IRTCore -> 0
        IRTMaster -> 1
        IRTTask -> 2
        InstanceRoleType' name -> (error . showText) $ "Unknown InstanceRoleType: " <> original name

-- | Represents the bounds of /known/ $InstanceRoleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceRoleType where
    minBound = IRTCore
    maxBound = IRTTask

instance Hashable     InstanceRoleType
instance NFData       InstanceRoleType
instance ToByteString InstanceRoleType
instance ToQuery      InstanceRoleType
instance ToHeader     InstanceRoleType

instance ToJSON InstanceRoleType where
    toJSON = toJSONText
