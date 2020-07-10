{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.JobStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.JobStatusType (
  JobStatusType (
    ..
    , Completed
    , Failed
    , InProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobStatusType = JobStatusType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Completed :: JobStatusType
pattern Completed = JobStatusType' "COMPLETED"

pattern Failed :: JobStatusType
pattern Failed = JobStatusType' "FAILED"

pattern InProgress :: JobStatusType
pattern InProgress = JobStatusType' "IN_PROGRESS"

{-# COMPLETE
  Completed,
  Failed,
  InProgress,
  JobStatusType' #-}

instance FromText JobStatusType where
    parser = (JobStatusType' . mk) <$> takeText

instance ToText JobStatusType where
    toText (JobStatusType' ci) = original ci

-- | Represents an enum of /known/ $JobStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobStatusType where
    toEnum i = case i of
        0 -> Completed
        1 -> Failed
        2 -> InProgress
        _ -> (error . showText) $ "Unknown index for JobStatusType: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Failed -> 1
        InProgress -> 2
        JobStatusType' name -> (error . showText) $ "Unknown JobStatusType: " <> original name

-- | Represents the bounds of /known/ $JobStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobStatusType where
    minBound = Completed
    maxBound = InProgress

instance Hashable     JobStatusType
instance NFData       JobStatusType
instance ToByteString JobStatusType
instance ToQuery      JobStatusType
instance ToHeader     JobStatusType

instance FromXML JobStatusType where
    parseXML = parseXMLText "JobStatusType"
