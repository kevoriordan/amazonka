{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionFailureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecutionFailureType (
  JobExecutionFailureType (
    ..
    , JEFTAll
    , JEFTFailed
    , JEFTRejected
    , JEFTTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobExecutionFailureType = JobExecutionFailureType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern JEFTAll :: JobExecutionFailureType
pattern JEFTAll = JobExecutionFailureType' "ALL"

pattern JEFTFailed :: JobExecutionFailureType
pattern JEFTFailed = JobExecutionFailureType' "FAILED"

pattern JEFTRejected :: JobExecutionFailureType
pattern JEFTRejected = JobExecutionFailureType' "REJECTED"

pattern JEFTTimedOut :: JobExecutionFailureType
pattern JEFTTimedOut = JobExecutionFailureType' "TIMED_OUT"

{-# COMPLETE
  JEFTAll,
  JEFTFailed,
  JEFTRejected,
  JEFTTimedOut,
  JobExecutionFailureType' #-}

instance FromText JobExecutionFailureType where
    parser = (JobExecutionFailureType' . mk) <$> takeText

instance ToText JobExecutionFailureType where
    toText (JobExecutionFailureType' ci) = original ci

-- | Represents an enum of /known/ $JobExecutionFailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobExecutionFailureType where
    toEnum i = case i of
        0 -> JEFTAll
        1 -> JEFTFailed
        2 -> JEFTRejected
        3 -> JEFTTimedOut
        _ -> (error . showText) $ "Unknown index for JobExecutionFailureType: " <> toText i
    fromEnum x = case x of
        JEFTAll -> 0
        JEFTFailed -> 1
        JEFTRejected -> 2
        JEFTTimedOut -> 3
        JobExecutionFailureType' name -> (error . showText) $ "Unknown JobExecutionFailureType: " <> original name

-- | Represents the bounds of /known/ $JobExecutionFailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobExecutionFailureType where
    minBound = JEFTAll
    maxBound = JEFTTimedOut

instance Hashable     JobExecutionFailureType
instance NFData       JobExecutionFailureType
instance ToByteString JobExecutionFailureType
instance ToQuery      JobExecutionFailureType
instance ToHeader     JobExecutionFailureType

instance ToJSON JobExecutionFailureType where
    toJSON = toJSONText

instance FromJSON JobExecutionFailureType where
    parseJSON = parseJSONText "JobExecutionFailureType"
