{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterSettingName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ClusterSettingName (
  ClusterSettingName (
    ..
    , ContainerInsights
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClusterSettingName = ClusterSettingName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ContainerInsights :: ClusterSettingName
pattern ContainerInsights = ClusterSettingName' "containerInsights"

{-# COMPLETE
  ContainerInsights,
  ClusterSettingName' #-}

instance FromText ClusterSettingName where
    parser = (ClusterSettingName' . mk) <$> takeText

instance ToText ClusterSettingName where
    toText (ClusterSettingName' ci) = original ci

-- | Represents an enum of /known/ $ClusterSettingName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClusterSettingName where
    toEnum i = case i of
        0 -> ContainerInsights
        _ -> (error . showText) $ "Unknown index for ClusterSettingName: " <> toText i
    fromEnum x = case x of
        ContainerInsights -> 0
        ClusterSettingName' name -> (error . showText) $ "Unknown ClusterSettingName: " <> original name

-- | Represents the bounds of /known/ $ClusterSettingName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClusterSettingName where
    minBound = ContainerInsights
    maxBound = ContainerInsights

instance Hashable     ClusterSettingName
instance NFData       ClusterSettingName
instance ToByteString ClusterSettingName
instance ToQuery      ClusterSettingName
instance ToHeader     ClusterSettingName

instance ToJSON ClusterSettingName where
    toJSON = toJSONText

instance FromJSON ClusterSettingName where
    parseJSON = parseJSONText "ClusterSettingName"
