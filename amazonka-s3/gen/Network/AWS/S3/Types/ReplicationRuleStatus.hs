{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationRuleStatus (
  ReplicationRuleStatus (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationRuleStatus = ReplicationRuleStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Disabled :: ReplicationRuleStatus
pattern Disabled = ReplicationRuleStatus' "Disabled"

pattern Enabled :: ReplicationRuleStatus
pattern Enabled = ReplicationRuleStatus' "Enabled"

{-# COMPLETE
  Disabled,
  Enabled,
  ReplicationRuleStatus' #-}

instance FromText ReplicationRuleStatus where
    parser = (ReplicationRuleStatus' . mk) <$> takeText

instance ToText ReplicationRuleStatus where
    toText (ReplicationRuleStatus' ci) = original ci

-- | Represents an enum of /known/ $ReplicationRuleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationRuleStatus where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for ReplicationRuleStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        ReplicationRuleStatus' name -> (error . showText) $ "Unknown ReplicationRuleStatus: " <> original name

-- | Represents the bounds of /known/ $ReplicationRuleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationRuleStatus where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     ReplicationRuleStatus
instance NFData       ReplicationRuleStatus
instance ToByteString ReplicationRuleStatus
instance ToQuery      ReplicationRuleStatus
instance ToHeader     ReplicationRuleStatus

instance FromXML ReplicationRuleStatus where
    parseXML = parseXMLText "ReplicationRuleStatus"

instance ToXML ReplicationRuleStatus where
    toXML = toXMLText
