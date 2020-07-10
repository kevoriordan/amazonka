{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentWaitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentWaitType (
  DeploymentWaitType (
    ..
    , ReadyWait
    , TerminationWait
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentWaitType = DeploymentWaitType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ReadyWait :: DeploymentWaitType
pattern ReadyWait = DeploymentWaitType' "READY_WAIT"

pattern TerminationWait :: DeploymentWaitType
pattern TerminationWait = DeploymentWaitType' "TERMINATION_WAIT"

{-# COMPLETE
  ReadyWait,
  TerminationWait,
  DeploymentWaitType' #-}

instance FromText DeploymentWaitType where
    parser = (DeploymentWaitType' . mk) <$> takeText

instance ToText DeploymentWaitType where
    toText (DeploymentWaitType' ci) = original ci

-- | Represents an enum of /known/ $DeploymentWaitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentWaitType where
    toEnum i = case i of
        0 -> ReadyWait
        1 -> TerminationWait
        _ -> (error . showText) $ "Unknown index for DeploymentWaitType: " <> toText i
    fromEnum x = case x of
        ReadyWait -> 0
        TerminationWait -> 1
        DeploymentWaitType' name -> (error . showText) $ "Unknown DeploymentWaitType: " <> original name

-- | Represents the bounds of /known/ $DeploymentWaitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentWaitType where
    minBound = ReadyWait
    maxBound = TerminationWait

instance Hashable     DeploymentWaitType
instance NFData       DeploymentWaitType
instance ToByteString DeploymentWaitType
instance ToQuery      DeploymentWaitType
instance ToHeader     DeploymentWaitType

instance ToJSON DeploymentWaitType where
    toJSON = toJSONText
