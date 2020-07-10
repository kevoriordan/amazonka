{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceFleetType (
  InstanceFleetType (
    ..
    , IFTCore
    , IFTMaster
    , IFTTask
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceFleetType = InstanceFleetType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern IFTCore :: InstanceFleetType
pattern IFTCore = InstanceFleetType' "CORE"

pattern IFTMaster :: InstanceFleetType
pattern IFTMaster = InstanceFleetType' "MASTER"

pattern IFTTask :: InstanceFleetType
pattern IFTTask = InstanceFleetType' "TASK"

{-# COMPLETE
  IFTCore,
  IFTMaster,
  IFTTask,
  InstanceFleetType' #-}

instance FromText InstanceFleetType where
    parser = (InstanceFleetType' . mk) <$> takeText

instance ToText InstanceFleetType where
    toText (InstanceFleetType' ci) = original ci

-- | Represents an enum of /known/ $InstanceFleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceFleetType where
    toEnum i = case i of
        0 -> IFTCore
        1 -> IFTMaster
        2 -> IFTTask
        _ -> (error . showText) $ "Unknown index for InstanceFleetType: " <> toText i
    fromEnum x = case x of
        IFTCore -> 0
        IFTMaster -> 1
        IFTTask -> 2
        InstanceFleetType' name -> (error . showText) $ "Unknown InstanceFleetType: " <> original name

-- | Represents the bounds of /known/ $InstanceFleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceFleetType where
    minBound = IFTCore
    maxBound = IFTTask

instance Hashable     InstanceFleetType
instance NFData       InstanceFleetType
instance ToByteString InstanceFleetType
instance ToQuery      InstanceFleetType
instance ToHeader     InstanceFleetType

instance ToJSON InstanceFleetType where
    toJSON = toJSONText

instance FromJSON InstanceFleetType where
    parseJSON = parseJSONText "InstanceFleetType"
