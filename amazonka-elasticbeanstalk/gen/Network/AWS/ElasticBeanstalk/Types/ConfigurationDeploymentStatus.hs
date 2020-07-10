{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus (
  ConfigurationDeploymentStatus (
    ..
    , CDSDeployed
    , CDSFailed
    , CDSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigurationDeploymentStatus = ConfigurationDeploymentStatus' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern CDSDeployed :: ConfigurationDeploymentStatus
pattern CDSDeployed = ConfigurationDeploymentStatus' "deployed"

pattern CDSFailed :: ConfigurationDeploymentStatus
pattern CDSFailed = ConfigurationDeploymentStatus' "failed"

pattern CDSPending :: ConfigurationDeploymentStatus
pattern CDSPending = ConfigurationDeploymentStatus' "pending"

{-# COMPLETE
  CDSDeployed,
  CDSFailed,
  CDSPending,
  ConfigurationDeploymentStatus' #-}

instance FromText ConfigurationDeploymentStatus where
    parser = (ConfigurationDeploymentStatus' . mk) <$> takeText

instance ToText ConfigurationDeploymentStatus where
    toText (ConfigurationDeploymentStatus' ci) = original ci

-- | Represents an enum of /known/ $ConfigurationDeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigurationDeploymentStatus where
    toEnum i = case i of
        0 -> CDSDeployed
        1 -> CDSFailed
        2 -> CDSPending
        _ -> (error . showText) $ "Unknown index for ConfigurationDeploymentStatus: " <> toText i
    fromEnum x = case x of
        CDSDeployed -> 0
        CDSFailed -> 1
        CDSPending -> 2
        ConfigurationDeploymentStatus' name -> (error . showText) $ "Unknown ConfigurationDeploymentStatus: " <> original name

-- | Represents the bounds of /known/ $ConfigurationDeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigurationDeploymentStatus where
    minBound = CDSDeployed
    maxBound = CDSPending

instance Hashable     ConfigurationDeploymentStatus
instance NFData       ConfigurationDeploymentStatus
instance ToByteString ConfigurationDeploymentStatus
instance ToQuery      ConfigurationDeploymentStatus
instance ToHeader     ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    parseXML = parseXMLText "ConfigurationDeploymentStatus"
