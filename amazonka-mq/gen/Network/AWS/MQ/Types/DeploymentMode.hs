{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.DeploymentMode (
  DeploymentMode (
    ..
    , ActiveStandbyMultiAz
    , SingleInstance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The deployment mode of the broker.
data DeploymentMode = DeploymentMode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ActiveStandbyMultiAz :: DeploymentMode
pattern ActiveStandbyMultiAz = DeploymentMode' "ACTIVE_STANDBY_MULTI_AZ"

pattern SingleInstance :: DeploymentMode
pattern SingleInstance = DeploymentMode' "SINGLE_INSTANCE"

{-# COMPLETE
  ActiveStandbyMultiAz,
  SingleInstance,
  DeploymentMode' #-}

instance FromText DeploymentMode where
    parser = (DeploymentMode' . mk) <$> takeText

instance ToText DeploymentMode where
    toText (DeploymentMode' ci) = original ci

-- | Represents an enum of /known/ $DeploymentMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentMode where
    toEnum i = case i of
        0 -> ActiveStandbyMultiAz
        1 -> SingleInstance
        _ -> (error . showText) $ "Unknown index for DeploymentMode: " <> toText i
    fromEnum x = case x of
        ActiveStandbyMultiAz -> 0
        SingleInstance -> 1
        DeploymentMode' name -> (error . showText) $ "Unknown DeploymentMode: " <> original name

-- | Represents the bounds of /known/ $DeploymentMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentMode where
    minBound = ActiveStandbyMultiAz
    maxBound = SingleInstance

instance Hashable     DeploymentMode
instance NFData       DeploymentMode
instance ToByteString DeploymentMode
instance ToQuery      DeploymentMode
instance ToHeader     DeploymentMode

instance ToJSON DeploymentMode where
    toJSON = toJSONText

instance FromJSON DeploymentMode where
    parseJSON = parseJSONText "DeploymentMode"
