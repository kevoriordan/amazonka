{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.StartReplicationTaskTypeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.StartReplicationTaskTypeValue (
  StartReplicationTaskTypeValue (
    ..
    , ReloadTarget
    , ResumeProcessing
    , StartReplication
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StartReplicationTaskTypeValue = StartReplicationTaskTypeValue' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern ReloadTarget :: StartReplicationTaskTypeValue
pattern ReloadTarget = StartReplicationTaskTypeValue' "reload-target"

pattern ResumeProcessing :: StartReplicationTaskTypeValue
pattern ResumeProcessing = StartReplicationTaskTypeValue' "resume-processing"

pattern StartReplication :: StartReplicationTaskTypeValue
pattern StartReplication = StartReplicationTaskTypeValue' "start-replication"

{-# COMPLETE
  ReloadTarget,
  ResumeProcessing,
  StartReplication,
  StartReplicationTaskTypeValue' #-}

instance FromText StartReplicationTaskTypeValue where
    parser = (StartReplicationTaskTypeValue' . mk) <$> takeText

instance ToText StartReplicationTaskTypeValue where
    toText (StartReplicationTaskTypeValue' ci) = original ci

-- | Represents an enum of /known/ $StartReplicationTaskTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StartReplicationTaskTypeValue where
    toEnum i = case i of
        0 -> ReloadTarget
        1 -> ResumeProcessing
        2 -> StartReplication
        _ -> (error . showText) $ "Unknown index for StartReplicationTaskTypeValue: " <> toText i
    fromEnum x = case x of
        ReloadTarget -> 0
        ResumeProcessing -> 1
        StartReplication -> 2
        StartReplicationTaskTypeValue' name -> (error . showText) $ "Unknown StartReplicationTaskTypeValue: " <> original name

-- | Represents the bounds of /known/ $StartReplicationTaskTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StartReplicationTaskTypeValue where
    minBound = ReloadTarget
    maxBound = StartReplication

instance Hashable     StartReplicationTaskTypeValue
instance NFData       StartReplicationTaskTypeValue
instance ToByteString StartReplicationTaskTypeValue
instance ToQuery      StartReplicationTaskTypeValue
instance ToHeader     StartReplicationTaskTypeValue

instance ToJSON StartReplicationTaskTypeValue where
    toJSON = toJSONText
