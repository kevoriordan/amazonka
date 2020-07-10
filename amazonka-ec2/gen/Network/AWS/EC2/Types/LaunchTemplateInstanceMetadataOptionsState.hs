{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState (
  LaunchTemplateInstanceMetadataOptionsState (
    ..
    , LTIMOSApplied
    , LTIMOSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateInstanceMetadataOptionsState = LaunchTemplateInstanceMetadataOptionsState' (CI
                                                                                                 Text)
                                                    deriving (Eq, Ord, Read,
                                                              Show, Data,
                                                              Typeable, Generic)

pattern LTIMOSApplied :: LaunchTemplateInstanceMetadataOptionsState
pattern LTIMOSApplied = LaunchTemplateInstanceMetadataOptionsState' "applied"

pattern LTIMOSPending :: LaunchTemplateInstanceMetadataOptionsState
pattern LTIMOSPending = LaunchTemplateInstanceMetadataOptionsState' "pending"

{-# COMPLETE
  LTIMOSApplied,
  LTIMOSPending,
  LaunchTemplateInstanceMetadataOptionsState' #-}

instance FromText LaunchTemplateInstanceMetadataOptionsState where
    parser = (LaunchTemplateInstanceMetadataOptionsState' . mk) <$> takeText

instance ToText LaunchTemplateInstanceMetadataOptionsState where
    toText (LaunchTemplateInstanceMetadataOptionsState' ci) = original ci

-- | Represents an enum of /known/ $LaunchTemplateInstanceMetadataOptionsState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LaunchTemplateInstanceMetadataOptionsState where
    toEnum i = case i of
        0 -> LTIMOSApplied
        1 -> LTIMOSPending
        _ -> (error . showText) $ "Unknown index for LaunchTemplateInstanceMetadataOptionsState: " <> toText i
    fromEnum x = case x of
        LTIMOSApplied -> 0
        LTIMOSPending -> 1
        LaunchTemplateInstanceMetadataOptionsState' name -> (error . showText) $ "Unknown LaunchTemplateInstanceMetadataOptionsState: " <> original name

-- | Represents the bounds of /known/ $LaunchTemplateInstanceMetadataOptionsState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LaunchTemplateInstanceMetadataOptionsState where
    minBound = LTIMOSApplied
    maxBound = LTIMOSPending

instance Hashable     LaunchTemplateInstanceMetadataOptionsState
instance NFData       LaunchTemplateInstanceMetadataOptionsState
instance ToByteString LaunchTemplateInstanceMetadataOptionsState
instance ToQuery      LaunchTemplateInstanceMetadataOptionsState
instance ToHeader     LaunchTemplateInstanceMetadataOptionsState

instance FromXML LaunchTemplateInstanceMetadataOptionsState where
    parseXML = parseXMLText "LaunchTemplateInstanceMetadataOptionsState"
