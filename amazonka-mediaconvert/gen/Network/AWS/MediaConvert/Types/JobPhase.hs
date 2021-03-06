{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobPhase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.JobPhase (
  JobPhase (
    ..
    , Probing
    , Transcoding
    , Uploading
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
data JobPhase = JobPhase' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Probing :: JobPhase
pattern Probing = JobPhase' "PROBING"

pattern Transcoding :: JobPhase
pattern Transcoding = JobPhase' "TRANSCODING"

pattern Uploading :: JobPhase
pattern Uploading = JobPhase' "UPLOADING"

{-# COMPLETE
  Probing,
  Transcoding,
  Uploading,
  JobPhase' #-}

instance FromText JobPhase where
    parser = (JobPhase' . mk) <$> takeText

instance ToText JobPhase where
    toText (JobPhase' ci) = original ci

-- | Represents an enum of /known/ $JobPhase.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobPhase where
    toEnum i = case i of
        0 -> Probing
        1 -> Transcoding
        2 -> Uploading
        _ -> (error . showText) $ "Unknown index for JobPhase: " <> toText i
    fromEnum x = case x of
        Probing -> 0
        Transcoding -> 1
        Uploading -> 2
        JobPhase' name -> (error . showText) $ "Unknown JobPhase: " <> original name

-- | Represents the bounds of /known/ $JobPhase.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobPhase where
    minBound = Probing
    maxBound = Uploading

instance Hashable     JobPhase
instance NFData       JobPhase
instance ToByteString JobPhase
instance ToQuery      JobPhase
instance ToHeader     JobPhase

instance FromJSON JobPhase where
    parseJSON = parseJSONText "JobPhase"
