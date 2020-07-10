{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition (
  CloudWatchLogsInitialPosition (
    ..
    , EndOfFile
    , StartOfFile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.
--
--
data CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern EndOfFile :: CloudWatchLogsInitialPosition
pattern EndOfFile = CloudWatchLogsInitialPosition' "end_of_file"

pattern StartOfFile :: CloudWatchLogsInitialPosition
pattern StartOfFile = CloudWatchLogsInitialPosition' "start_of_file"

{-# COMPLETE
  EndOfFile,
  StartOfFile,
  CloudWatchLogsInitialPosition' #-}

instance FromText CloudWatchLogsInitialPosition where
    parser = (CloudWatchLogsInitialPosition' . mk) <$> takeText

instance ToText CloudWatchLogsInitialPosition where
    toText (CloudWatchLogsInitialPosition' ci) = original ci

-- | Represents an enum of /known/ $CloudWatchLogsInitialPosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudWatchLogsInitialPosition where
    toEnum i = case i of
        0 -> EndOfFile
        1 -> StartOfFile
        _ -> (error . showText) $ "Unknown index for CloudWatchLogsInitialPosition: " <> toText i
    fromEnum x = case x of
        EndOfFile -> 0
        StartOfFile -> 1
        CloudWatchLogsInitialPosition' name -> (error . showText) $ "Unknown CloudWatchLogsInitialPosition: " <> original name

-- | Represents the bounds of /known/ $CloudWatchLogsInitialPosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudWatchLogsInitialPosition where
    minBound = EndOfFile
    maxBound = StartOfFile

instance Hashable     CloudWatchLogsInitialPosition
instance NFData       CloudWatchLogsInitialPosition
instance ToByteString CloudWatchLogsInitialPosition
instance ToQuery      CloudWatchLogsInitialPosition
instance ToHeader     CloudWatchLogsInitialPosition

instance ToJSON CloudWatchLogsInitialPosition where
    toJSON = toJSONText

instance FromJSON CloudWatchLogsInitialPosition where
    parseJSON = parseJSONText "CloudWatchLogsInitialPosition"
