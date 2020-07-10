{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ProcessorParameterName (
  ProcessorParameterName (
    ..
    , BufferIntervalInSeconds
    , BufferSizeInMBs
    , LambdaARN
    , NumberOfRetries
    , RoleARN
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessorParameterName = ProcessorParameterName' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern BufferIntervalInSeconds :: ProcessorParameterName
pattern BufferIntervalInSeconds = ProcessorParameterName' "BufferIntervalInSeconds"

pattern BufferSizeInMBs :: ProcessorParameterName
pattern BufferSizeInMBs = ProcessorParameterName' "BufferSizeInMBs"

pattern LambdaARN :: ProcessorParameterName
pattern LambdaARN = ProcessorParameterName' "LambdaArn"

pattern NumberOfRetries :: ProcessorParameterName
pattern NumberOfRetries = ProcessorParameterName' "NumberOfRetries"

pattern RoleARN :: ProcessorParameterName
pattern RoleARN = ProcessorParameterName' "RoleArn"

{-# COMPLETE
  BufferIntervalInSeconds,
  BufferSizeInMBs,
  LambdaARN,
  NumberOfRetries,
  RoleARN,
  ProcessorParameterName' #-}

instance FromText ProcessorParameterName where
    parser = (ProcessorParameterName' . mk) <$> takeText

instance ToText ProcessorParameterName where
    toText (ProcessorParameterName' ci) = original ci

-- | Represents an enum of /known/ $ProcessorParameterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessorParameterName where
    toEnum i = case i of
        0 -> BufferIntervalInSeconds
        1 -> BufferSizeInMBs
        2 -> LambdaARN
        3 -> NumberOfRetries
        4 -> RoleARN
        _ -> (error . showText) $ "Unknown index for ProcessorParameterName: " <> toText i
    fromEnum x = case x of
        BufferIntervalInSeconds -> 0
        BufferSizeInMBs -> 1
        LambdaARN -> 2
        NumberOfRetries -> 3
        RoleARN -> 4
        ProcessorParameterName' name -> (error . showText) $ "Unknown ProcessorParameterName: " <> original name

-- | Represents the bounds of /known/ $ProcessorParameterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessorParameterName where
    minBound = BufferIntervalInSeconds
    maxBound = RoleARN

instance Hashable     ProcessorParameterName
instance NFData       ProcessorParameterName
instance ToByteString ProcessorParameterName
instance ToQuery      ProcessorParameterName
instance ToHeader     ProcessorParameterName

instance ToJSON ProcessorParameterName where
    toJSON = toJSONText

instance FromJSON ProcessorParameterName where
    parseJSON = parseJSONText "ProcessorParameterName"
