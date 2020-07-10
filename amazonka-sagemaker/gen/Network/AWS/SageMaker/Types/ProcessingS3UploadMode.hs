{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3UploadMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3UploadMode (
  ProcessingS3UploadMode (
    ..
    , Continuous
    , EndOfJob
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingS3UploadMode = ProcessingS3UploadMode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Continuous :: ProcessingS3UploadMode
pattern Continuous = ProcessingS3UploadMode' "Continuous"

pattern EndOfJob :: ProcessingS3UploadMode
pattern EndOfJob = ProcessingS3UploadMode' "EndOfJob"

{-# COMPLETE
  Continuous,
  EndOfJob,
  ProcessingS3UploadMode' #-}

instance FromText ProcessingS3UploadMode where
    parser = (ProcessingS3UploadMode' . mk) <$> takeText

instance ToText ProcessingS3UploadMode where
    toText (ProcessingS3UploadMode' ci) = original ci

-- | Represents an enum of /known/ $ProcessingS3UploadMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingS3UploadMode where
    toEnum i = case i of
        0 -> Continuous
        1 -> EndOfJob
        _ -> (error . showText) $ "Unknown index for ProcessingS3UploadMode: " <> toText i
    fromEnum x = case x of
        Continuous -> 0
        EndOfJob -> 1
        ProcessingS3UploadMode' name -> (error . showText) $ "Unknown ProcessingS3UploadMode: " <> original name

-- | Represents the bounds of /known/ $ProcessingS3UploadMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingS3UploadMode where
    minBound = Continuous
    maxBound = EndOfJob

instance Hashable     ProcessingS3UploadMode
instance NFData       ProcessingS3UploadMode
instance ToByteString ProcessingS3UploadMode
instance ToQuery      ProcessingS3UploadMode
instance ToHeader     ProcessingS3UploadMode

instance ToJSON ProcessingS3UploadMode where
    toJSON = toJSONText

instance FromJSON ProcessingS3UploadMode where
    parseJSON = parseJSONText "ProcessingS3UploadMode"
