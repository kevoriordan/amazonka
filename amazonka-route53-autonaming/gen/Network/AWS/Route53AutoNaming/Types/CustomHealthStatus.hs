{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.CustomHealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.CustomHealthStatus (
  CustomHealthStatus (
    ..
    , CHSHealthy
    , CHSUnhealthy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CustomHealthStatus = CustomHealthStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern CHSHealthy :: CustomHealthStatus
pattern CHSHealthy = CustomHealthStatus' "HEALTHY"

pattern CHSUnhealthy :: CustomHealthStatus
pattern CHSUnhealthy = CustomHealthStatus' "UNHEALTHY"

{-# COMPLETE
  CHSHealthy,
  CHSUnhealthy,
  CustomHealthStatus' #-}

instance FromText CustomHealthStatus where
    parser = (CustomHealthStatus' . mk) <$> takeText

instance ToText CustomHealthStatus where
    toText (CustomHealthStatus' ci) = original ci

-- | Represents an enum of /known/ $CustomHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CustomHealthStatus where
    toEnum i = case i of
        0 -> CHSHealthy
        1 -> CHSUnhealthy
        _ -> (error . showText) $ "Unknown index for CustomHealthStatus: " <> toText i
    fromEnum x = case x of
        CHSHealthy -> 0
        CHSUnhealthy -> 1
        CustomHealthStatus' name -> (error . showText) $ "Unknown CustomHealthStatus: " <> original name

-- | Represents the bounds of /known/ $CustomHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CustomHealthStatus where
    minBound = CHSHealthy
    maxBound = CHSUnhealthy

instance Hashable     CustomHealthStatus
instance NFData       CustomHealthStatus
instance ToByteString CustomHealthStatus
instance ToQuery      CustomHealthStatus
instance ToHeader     CustomHealthStatus

instance ToJSON CustomHealthStatus where
    toJSON = toJSONText
