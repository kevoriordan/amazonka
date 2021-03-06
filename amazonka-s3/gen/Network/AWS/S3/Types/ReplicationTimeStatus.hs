{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTimeStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationTimeStatus (
  ReplicationTimeStatus (
    ..
    , RTSDisabled
    , RTSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationTimeStatus = ReplicationTimeStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern RTSDisabled :: ReplicationTimeStatus
pattern RTSDisabled = ReplicationTimeStatus' "Disabled"

pattern RTSEnabled :: ReplicationTimeStatus
pattern RTSEnabled = ReplicationTimeStatus' "Enabled"

{-# COMPLETE
  RTSDisabled,
  RTSEnabled,
  ReplicationTimeStatus' #-}

instance FromText ReplicationTimeStatus where
    parser = (ReplicationTimeStatus' . mk) <$> takeText

instance ToText ReplicationTimeStatus where
    toText (ReplicationTimeStatus' ci) = original ci

-- | Represents an enum of /known/ $ReplicationTimeStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationTimeStatus where
    toEnum i = case i of
        0 -> RTSDisabled
        1 -> RTSEnabled
        _ -> (error . showText) $ "Unknown index for ReplicationTimeStatus: " <> toText i
    fromEnum x = case x of
        RTSDisabled -> 0
        RTSEnabled -> 1
        ReplicationTimeStatus' name -> (error . showText) $ "Unknown ReplicationTimeStatus: " <> original name

-- | Represents the bounds of /known/ $ReplicationTimeStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationTimeStatus where
    minBound = RTSDisabled
    maxBound = RTSEnabled

instance Hashable     ReplicationTimeStatus
instance NFData       ReplicationTimeStatus
instance ToByteString ReplicationTimeStatus
instance ToQuery      ReplicationTimeStatus
instance ToHeader     ReplicationTimeStatus

instance FromXML ReplicationTimeStatus where
    parseXML = parseXMLText "ReplicationTimeStatus"

instance ToXML ReplicationTimeStatus where
    toXML = toXMLText
