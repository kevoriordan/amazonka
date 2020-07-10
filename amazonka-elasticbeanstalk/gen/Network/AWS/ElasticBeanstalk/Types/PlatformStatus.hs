{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.PlatformStatus (
  PlatformStatus (
    ..
    , Creating
    , Deleted
    , Deleting
    , Failed
    , Ready
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PlatformStatus = PlatformStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Creating :: PlatformStatus
pattern Creating = PlatformStatus' "Creating"

pattern Deleted :: PlatformStatus
pattern Deleted = PlatformStatus' "Deleted"

pattern Deleting :: PlatformStatus
pattern Deleting = PlatformStatus' "Deleting"

pattern Failed :: PlatformStatus
pattern Failed = PlatformStatus' "Failed"

pattern Ready :: PlatformStatus
pattern Ready = PlatformStatus' "Ready"

{-# COMPLETE
  Creating,
  Deleted,
  Deleting,
  Failed,
  Ready,
  PlatformStatus' #-}

instance FromText PlatformStatus where
    parser = (PlatformStatus' . mk) <$> takeText

instance ToText PlatformStatus where
    toText (PlatformStatus' ci) = original ci

-- | Represents an enum of /known/ $PlatformStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlatformStatus where
    toEnum i = case i of
        0 -> Creating
        1 -> Deleted
        2 -> Deleting
        3 -> Failed
        4 -> Ready
        _ -> (error . showText) $ "Unknown index for PlatformStatus: " <> toText i
    fromEnum x = case x of
        Creating -> 0
        Deleted -> 1
        Deleting -> 2
        Failed -> 3
        Ready -> 4
        PlatformStatus' name -> (error . showText) $ "Unknown PlatformStatus: " <> original name

-- | Represents the bounds of /known/ $PlatformStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlatformStatus where
    minBound = Creating
    maxBound = Ready

instance Hashable     PlatformStatus
instance NFData       PlatformStatus
instance ToByteString PlatformStatus
instance ToQuery      PlatformStatus
instance ToHeader     PlatformStatus

instance FromXML PlatformStatus where
    parseXML = parseXMLText "PlatformStatus"
