{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.DependentServiceName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.DependentServiceName (
  DependentServiceName (
    ..
    , AWSSHieldAdvanced
    , AWSconfig
    , AWSvpc
    , AWSwaf
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DependentServiceName = DependentServiceName' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AWSSHieldAdvanced :: DependentServiceName
pattern AWSSHieldAdvanced = DependentServiceName' "AWSSHIELD_ADVANCED"

pattern AWSconfig :: DependentServiceName
pattern AWSconfig = DependentServiceName' "AWSCONFIG"

pattern AWSvpc :: DependentServiceName
pattern AWSvpc = DependentServiceName' "AWSVPC"

pattern AWSwaf :: DependentServiceName
pattern AWSwaf = DependentServiceName' "AWSWAF"

{-# COMPLETE
  AWSSHieldAdvanced,
  AWSconfig,
  AWSvpc,
  AWSwaf,
  DependentServiceName' #-}

instance FromText DependentServiceName where
    parser = (DependentServiceName' . mk) <$> takeText

instance ToText DependentServiceName where
    toText (DependentServiceName' ci) = original ci

-- | Represents an enum of /known/ $DependentServiceName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DependentServiceName where
    toEnum i = case i of
        0 -> AWSSHieldAdvanced
        1 -> AWSconfig
        2 -> AWSvpc
        3 -> AWSwaf
        _ -> (error . showText) $ "Unknown index for DependentServiceName: " <> toText i
    fromEnum x = case x of
        AWSSHieldAdvanced -> 0
        AWSconfig -> 1
        AWSvpc -> 2
        AWSwaf -> 3
        DependentServiceName' name -> (error . showText) $ "Unknown DependentServiceName: " <> original name

-- | Represents the bounds of /known/ $DependentServiceName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DependentServiceName where
    minBound = AWSSHieldAdvanced
    maxBound = AWSwaf

instance Hashable     DependentServiceName
instance NFData       DependentServiceName
instance ToByteString DependentServiceName
instance ToQuery      DependentServiceName
instance ToHeader     DependentServiceName

instance FromJSON DependentServiceName where
    parseJSON = parseJSONText "DependentServiceName"
