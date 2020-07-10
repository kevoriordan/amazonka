{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.MetricsName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.MetricsName (
  MetricsName (
    ..
    , All
    , IncomingBytes
    , IncomingRecords
    , IteratorAgeMilliseconds
    , OutgoingBytes
    , OutgoingRecords
    , ReadProvisionedThroughputExceeded
    , WriteProvisionedThroughputExceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricsName = MetricsName' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern All :: MetricsName
pattern All = MetricsName' "ALL"

pattern IncomingBytes :: MetricsName
pattern IncomingBytes = MetricsName' "IncomingBytes"

pattern IncomingRecords :: MetricsName
pattern IncomingRecords = MetricsName' "IncomingRecords"

pattern IteratorAgeMilliseconds :: MetricsName
pattern IteratorAgeMilliseconds = MetricsName' "IteratorAgeMilliseconds"

pattern OutgoingBytes :: MetricsName
pattern OutgoingBytes = MetricsName' "OutgoingBytes"

pattern OutgoingRecords :: MetricsName
pattern OutgoingRecords = MetricsName' "OutgoingRecords"

pattern ReadProvisionedThroughputExceeded :: MetricsName
pattern ReadProvisionedThroughputExceeded = MetricsName' "ReadProvisionedThroughputExceeded"

pattern WriteProvisionedThroughputExceeded :: MetricsName
pattern WriteProvisionedThroughputExceeded = MetricsName' "WriteProvisionedThroughputExceeded"

{-# COMPLETE
  All,
  IncomingBytes,
  IncomingRecords,
  IteratorAgeMilliseconds,
  OutgoingBytes,
  OutgoingRecords,
  ReadProvisionedThroughputExceeded,
  WriteProvisionedThroughputExceeded,
  MetricsName' #-}

instance FromText MetricsName where
    parser = (MetricsName' . mk) <$> takeText

instance ToText MetricsName where
    toText (MetricsName' ci) = original ci

-- | Represents an enum of /known/ $MetricsName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricsName where
    toEnum i = case i of
        0 -> All
        1 -> IncomingBytes
        2 -> IncomingRecords
        3 -> IteratorAgeMilliseconds
        4 -> OutgoingBytes
        5 -> OutgoingRecords
        6 -> ReadProvisionedThroughputExceeded
        7 -> WriteProvisionedThroughputExceeded
        _ -> (error . showText) $ "Unknown index for MetricsName: " <> toText i
    fromEnum x = case x of
        All -> 0
        IncomingBytes -> 1
        IncomingRecords -> 2
        IteratorAgeMilliseconds -> 3
        OutgoingBytes -> 4
        OutgoingRecords -> 5
        ReadProvisionedThroughputExceeded -> 6
        WriteProvisionedThroughputExceeded -> 7
        MetricsName' name -> (error . showText) $ "Unknown MetricsName: " <> original name

-- | Represents the bounds of /known/ $MetricsName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricsName where
    minBound = All
    maxBound = WriteProvisionedThroughputExceeded

instance Hashable     MetricsName
instance NFData       MetricsName
instance ToByteString MetricsName
instance ToQuery      MetricsName
instance ToHeader     MetricsName

instance ToJSON MetricsName where
    toJSON = toJSONText

instance FromJSON MetricsName where
    parseJSON = parseJSONText "MetricsName"
