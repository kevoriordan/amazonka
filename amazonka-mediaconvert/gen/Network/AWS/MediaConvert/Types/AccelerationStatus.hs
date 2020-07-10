{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AccelerationStatus (
  AccelerationStatus (
    ..
    , Accelerated
    , InProgress
    , NotAccelerated
    , NotApplicable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
data AccelerationStatus = AccelerationStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Accelerated :: AccelerationStatus
pattern Accelerated = AccelerationStatus' "ACCELERATED"

pattern InProgress :: AccelerationStatus
pattern InProgress = AccelerationStatus' "IN_PROGRESS"

pattern NotAccelerated :: AccelerationStatus
pattern NotAccelerated = AccelerationStatus' "NOT_ACCELERATED"

pattern NotApplicable :: AccelerationStatus
pattern NotApplicable = AccelerationStatus' "NOT_APPLICABLE"

{-# COMPLETE
  Accelerated,
  InProgress,
  NotAccelerated,
  NotApplicable,
  AccelerationStatus' #-}

instance FromText AccelerationStatus where
    parser = (AccelerationStatus' . mk) <$> takeText

instance ToText AccelerationStatus where
    toText (AccelerationStatus' ci) = original ci

-- | Represents an enum of /known/ $AccelerationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccelerationStatus where
    toEnum i = case i of
        0 -> Accelerated
        1 -> InProgress
        2 -> NotAccelerated
        3 -> NotApplicable
        _ -> (error . showText) $ "Unknown index for AccelerationStatus: " <> toText i
    fromEnum x = case x of
        Accelerated -> 0
        InProgress -> 1
        NotAccelerated -> 2
        NotApplicable -> 3
        AccelerationStatus' name -> (error . showText) $ "Unknown AccelerationStatus: " <> original name

-- | Represents the bounds of /known/ $AccelerationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccelerationStatus where
    minBound = Accelerated
    maxBound = NotApplicable

instance Hashable     AccelerationStatus
instance NFData       AccelerationStatus
instance ToByteString AccelerationStatus
instance ToQuery      AccelerationStatus
instance ToHeader     AccelerationStatus

instance FromJSON AccelerationStatus where
    parseJSON = parseJSONText "AccelerationStatus"
