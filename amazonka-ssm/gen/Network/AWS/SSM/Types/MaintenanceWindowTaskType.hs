{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowTaskType (
  MaintenanceWindowTaskType (
    ..
    , Automation
    , Lambda
    , RunCommand
    , StepFunctions
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MaintenanceWindowTaskType = MaintenanceWindowTaskType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Automation :: MaintenanceWindowTaskType
pattern Automation = MaintenanceWindowTaskType' "AUTOMATION"

pattern Lambda :: MaintenanceWindowTaskType
pattern Lambda = MaintenanceWindowTaskType' "LAMBDA"

pattern RunCommand :: MaintenanceWindowTaskType
pattern RunCommand = MaintenanceWindowTaskType' "RUN_COMMAND"

pattern StepFunctions :: MaintenanceWindowTaskType
pattern StepFunctions = MaintenanceWindowTaskType' "STEP_FUNCTIONS"

{-# COMPLETE
  Automation,
  Lambda,
  RunCommand,
  StepFunctions,
  MaintenanceWindowTaskType' #-}

instance FromText MaintenanceWindowTaskType where
    parser = (MaintenanceWindowTaskType' . mk) <$> takeText

instance ToText MaintenanceWindowTaskType where
    toText (MaintenanceWindowTaskType' ci) = original ci

-- | Represents an enum of /known/ $MaintenanceWindowTaskType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MaintenanceWindowTaskType where
    toEnum i = case i of
        0 -> Automation
        1 -> Lambda
        2 -> RunCommand
        3 -> StepFunctions
        _ -> (error . showText) $ "Unknown index for MaintenanceWindowTaskType: " <> toText i
    fromEnum x = case x of
        Automation -> 0
        Lambda -> 1
        RunCommand -> 2
        StepFunctions -> 3
        MaintenanceWindowTaskType' name -> (error . showText) $ "Unknown MaintenanceWindowTaskType: " <> original name

-- | Represents the bounds of /known/ $MaintenanceWindowTaskType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MaintenanceWindowTaskType where
    minBound = Automation
    maxBound = StepFunctions

instance Hashable     MaintenanceWindowTaskType
instance NFData       MaintenanceWindowTaskType
instance ToByteString MaintenanceWindowTaskType
instance ToQuery      MaintenanceWindowTaskType
instance ToHeader     MaintenanceWindowTaskType

instance ToJSON MaintenanceWindowTaskType where
    toJSON = toJSONText

instance FromJSON MaintenanceWindowTaskType where
    parseJSON = parseJSONText "MaintenanceWindowTaskType"
